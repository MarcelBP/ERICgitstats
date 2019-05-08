(ns codescene.presentation.system-health
  (:require
   [codescene.presentation.health :as health]
   [codescene.presentation.display :as display]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.set :as cset]
   [taoensso.timbre :as log]
   [semantic-csv.core :as sc]))



(def ^:private placeholder-for-unknown-data "-")

;;; TODO: the thresholds here could be parameters instead of magic numbers.
(defn- calculate-system-health-statuses
  [{:keys             [team-autonomy
                       system-mastery
                       current-score
                       defects-now
                       delivery-risk] :as health}]
  (cond-> health
    team-autonomy
    (assoc :team-autonomy-status (get {"High"   "good"
                                       "Medium" "normal"
                                       "Low"    "bad"
                                       "-"      "not-calculated"} team-autonomy))
    system-mastery
    (assoc :system-mastery-status (health/val->status system-mastery 30 70))
    current-score
    (assoc :current-score-status (health/val->status current-score 3 8))
    defects-now
    (assoc :defects-now-status (health/val->status (if (int? defects-now) (- 100 defects-now) defects-now) 30 70))
    delivery-risk
    (assoc :delivery-risk-status (health/delivery-risk-val->status delivery-risk))))



(defn- approx=
  "Assumes positive integers. Risk of division by 0 if negative integers
  are allowed. `precision` is a decimal value between 0 and 1. So for
  20% precision, use 0.2. In this case `(approx= 0.2 99 81)` would be true."
  ([a b] (approx= 0.1 a b))
  ([precision a b]
   (or (= a b)                          
       (< (- 1 (double (/ (min a b) (max a b)))) precision))))


(defn- single-defect-trend
  [now other]
  (cond
    (some #(= "-" %) [now other]) "-" ; no data
    (approx= now other)           0
    (> now other)                 -1
    ::otherwise-improving         1))

;;; TODO: We are not sure whether to use both defect-month and
;;; defect-year, or only defect-month. We are going with only
;;; defect-month for now, but leaving this old dual-value version of
;;; `defects->trend` for reference, and continuing to use a vector to
;;; contain the trend results.
(comment
  (defn defects->trend
    [now month year]
    (if (= "-" now)
      [ "-"  "-"]
      [(single-defect-trend now month) (single-defect-trend now year)])))

(defn defects->trend
  [now month]
  (if (= "-" now)
    ["-"]
    [(single-defect-trend now month)]))

(defn- single-score-trend
  [now other]
  (if (some #(= "-" %) [now other])
    "-"
    (let [diff (- now other)
          abs-diff (Math/abs diff)
          diff-sign (cond (pos? diff) 1
                          (neg? diff) -1
                          ::zero 0)]
      (cond (zero? abs-diff) 0
            (< abs-diff 2) (* 1 diff-sign) 
            (<= 2 abs-diff 5) (* 2 diff-sign)
            (<= 5 abs-diff) (* 3 diff-sign )))))

(defn scores->trend
  [now month year]
  (if (= "-" now)
    ["-" "-"]
    [(single-score-trend now month)
     (single-score-trend now year)]))

(defn- calculate-trends
  [{:keys [current-score
           month-score
           year-score
           defects-now
           defects-month] :as health}]
  (cond-> (assoc health :score-trend-data (scores->trend current-score month-score year-score)) 
    defects-now
    (assoc :defect-trend-data (defects->trend defects-now defects-month)))) ;; add defects-year to this call if we decide to use defects-year

(defn- defect-trend-for-temporal-scope
  [[month] temporal-scope]
  (let [
        ;; This was the dual-value version: 
        ;; trend-val (get {"month" month "year" year} temporal-scope)
        trend-val month]
    (cond
      (nil? trend-val)  "not-calculated"
      (= "-" trend-val) "not-calculated"
      ;; trend-val is either -1 0 or 1. By incrementing we get the
      ;; index of the corresponding string.
      ::integer
      (nth ["worse" "stable" "better"] (inc trend-val)))))


(defn- nuanced-trend-for-temporal-scope
  "Returns one of: not-calculated, stable, or small-up, small-down,
  medium-up, medium-down, big-up, big-down."
  [[month year] temporal-scope]
  (let [trend-val (get {"month" month "year" year} temporal-scope)]
    (cond (= "-" trend-val) "not-calculated"
          (nil? trend-val) "not-calculated"
          (zero? trend-val) "stable"
          ::otherwise
          (let [size (get {1 "small" 2 "medium" 3 "big"} (Math/abs trend-val))
                direction (if (pos? trend-val) "up" "down")]
            (str size "-" direction )))))

(defn- select-temporal-scope
  [{:keys [score-trend-data defect-trend-data] :as health} temporal-scope]
  (cond-> (assoc health :score-trend (nuanced-trend-for-temporal-scope score-trend-data temporal-scope))
    defect-trend-data
    (assoc :defect-trend (defect-trend-for-temporal-scope defect-trend-data temporal-scope))))




(defn- zero-scores->not-calculated
  [{:keys [current-score month-score year-score] :as health}]
  (-> health
      (update :current-score health/zero->dash)
      (update :month-score health/zero->dash)
      (update :year-score health/zero->dash)))

(def ^:private hyphenated-column-names
  {:current :current-score
   :month :month-score
   :year :year-score
   :defects :defects-now
   :defectsmonth :defects-month
   :defectsyear :defects-year
   :mastery :system-mastery
   :teamautonomy :team-autonomy
   :deliveryrisk :delivery-risk})



(defn percentage->temperature-class [percentage]
  (cond
    (not (int? percentage)) "not-calculated"
    (<= percentage 20)      "very-cold"
    (<= 21 percentage 40)   "cold"
    (<= 41 percentage 60)   "warm"
    (<= 61 percentage 80)   "very-warm"
    (<= 81 percentage)      "hot"
    ::otherwise             "not-calculated"))


(defn- min-max-revisions [rows]
  (let [revisions  (map (comp display/as-int :revisions) rows)
        max-revisions (apply max 0 revisions)
        min-revisions (if (seq revisions)
                        (apply min revisions)
                        0)]
    [min-revisions max-revisions]))

(defn- calculate-activity-temperature
  [{:keys [revisions] :as row} min-revisions max-revisions]
  (let [percentage (if (zero? max-revisions)
                     placeholder-for-unknown-data
                     (int (* 100 (/ revisions max-revisions))))]
    (assoc row :development-activity-status (percentage->temperature-class percentage))))


(defn csv-rows->system-health
  [rows temporal-scope]
  (let [[min-revisions max-revisions] (min-max-revisions rows)]
    (->> rows
         (map-indexed (fn [idx row]
                        (-> row
                            (cset/rename-keys hyphenated-column-names)
                            display/to-ints-when-possible
                            zero-scores->not-calculated
                            (assoc :rank idx)  
                            calculate-system-health-statuses
                            calculate-trends
                            (calculate-activity-temperature min-revisions max-revisions)
                            (select-temporal-scope temporal-scope)))))))

(defn project->system-health
  [p]
  (-> p
      display/to-ints-when-possible
      zero-scores->not-calculated
      calculate-system-health-statuses
      calculate-trends
      (select-temporal-scope "month")))


(defn- more-than?
  "Scores might be '-', so interpret any non-int as 0."
  [a b]
  (let [a-int (if (int? a) a 0)
        b-int (if (int? b) b 0)]
    (> a-int b-int)))

(defn mark-updated-system-health-scores
  "Given a map of the previous scores
  (component-name -> {:current-score x :defects-now x})
  adds a :scrore-diff key to the system-health "
  [system-health-scores {:keys [components analysis-id] :as system-health-params}]
  (map
    (fn [{:keys [name current-score defects-now] :as component-scores}]
      (if-let [{previous-score :current-score
                defects-then :defects-now} (get components name)]
        (cond-> component-scores
          (more-than? current-score previous-score) (assoc :score-diff "real-time-better")
          (more-than? previous-score current-score) (assoc :score-diff "real-time-worse")
          (more-than? defects-then defects-now) (assoc :defects-diff "real-time-better")
          (more-than? defects-now defects-then) (assoc :defects-diff "real-time-worse"))
        component-scores))
    system-health-scores))






