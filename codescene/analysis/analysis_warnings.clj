(ns codescene.analysis.analysis-warnings
  (:require [cheshire.core :as json]
            [clojure.data.json :as json-clj]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [codescene.analysis.closed-maat-proxy :as maat]
            [codescene.analysis.paths :as paths]
            [codescene.analysis.specs :as specs]
            [codescene.analysis.versioning :as versioning]
            [codescene.risks.prediction :as risk-prediciton]
            [codescene.branches.branch-statistics :as branch-statistics]
            [codescene.thresholds.analysis-thresholds :as analysis-thresholds]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [evolutionary-metrics.trends.author-contribution :as author-contribution]
            [evolutionary-metrics.trends.hotspot-trends :as hotspot-trends]
            [taoensso.timbre :as log]))

;;
;; Hotspots warnings
;;

(defn- past-revisions-analysis-for-trends
  [{:keys [trend-log analysis-path-fn]} dest-file-name]
  (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))]
    (maat/run-closed-maat trend-log "revisions" destination {}) ; side effect
     destination))

(defn- check-rising-hotspot-warning
  [{:keys [analysis-path-fn] :as context} thresholds]
  (let [new-hotspots (analysis-path-fn paths/hotspots-csv)
        old-hotspots (past-revisions-analysis-for-trends context paths/trend-revisions-csv)
        results (analysis-path-fn paths/warning-hotspots-csv)]
    (log/trace "Detecting Rising Hotspots with thresholds: " (pr-str thresholds))
    (hotspot-trends/ranked-hotspot-delta old-hotspots
                                         new-hotspots
                                         thresholds
                                         results)))

(defn- generate-early-warnings-for-hotspots
  "Detects possible hotspots that rise rapidly on the ranking.
   NOTE: we cannot do rising spots detection when configured for
   relative churn as hotspot weight. The reason is that the weight
   is balanced by LoC and we don't want to rewind the Git repo of
   the user to calculate historic LoC."
  [{:keys [analysis-path-fn] :as context}]
  (let [new-hotspots (analysis-path-fn paths/hotspots-csv)
        hotspot-stats (analysis-thresholds/describe-hotspots new-hotspots)
        warning-thresholds (analysis-thresholds/rising-hotspots-warning hotspot-stats)
        spots-rising-above-threshold (check-rising-hotspot-warning context warning-thresholds)]
    (when (seq spots-rising-above-threshold)
      {::warning-type ::rising-hotspots
       ::hotspots spots-rising-above-threshold})))

;;
;; Complexity Trend warnings
;;

(defn- complexity-warning-fields
  [spots]
  (map last spots)) ; the final field is the warning - TAKE CARE!

(defn sum-complexity-trend-warnings-by-category
  [all-spots-warning-category]
  (reduce (fn [[acc-yellow acc-red] v]
            (let [red-warning (if (= "Red" v) 1 0)
                  yellow-warning (if (= "Yellow" v) 1 0)]
              [(+ acc-yellow yellow-warning) (+ acc-red red-warning)]))
          [0 0]
          all-spots-warning-category))

(defn- sum-complexity-trend-warnings
  [{:keys [analysis-path-fn]}]
  (let [all-spots (analysis-path-fn paths/augmented-hotspots-csv)
        all-spots-warning-category (complexity-warning-fields (shared/read-csv-sans-header-from all-spots))]
    (sum-complexity-trend-warnings-by-category all-spots-warning-category)))

(defn- generate-early-warnings-for-complexity-trends
  [context]
  (let [[yellow-markers red-markers] (sum-complexity-trend-warnings context)]
    (when (or (pos? yellow-markers)
              (pos? red-markers))
      {::warning-type ::complexity-increase
       ::yellow-markers yellow-markers
       ::red-markers red-markers})))

;;
;; Ex-developers warning
;;

(defn- known-ex-developers
  [analysis-path-fn]
  (->> (analysis-path-fn paths/lost-authors-csv)
       shared/read-csv-sans-header-from
       (map first)))

(defn- generate-early-warnings-for-ex-developers
  "Detects possible developers who haven't contributed in a long time (configurable).
   The idea is that you may have knowledge loss that you should be aware of."
  [{:keys [analysis-path-fn time-now]} {:keys [minexdevcontribtime exdevcontribenabled]}]
  (when (= 1 exdevcontribenabled)
    (let [ex-devs (known-ex-developers analysis-path-fn)
          potential-ex-devs (author-contribution/detect-authors-without-contribution
                              {:date-now time-now :time-in-months minexdevcontribtime}
                              (analysis-path-fn paths/author-churn-csv)
                              ex-devs
                              (analysis-path-fn paths/detected-ex-devs-warning-csv))]
      (when (seq potential-ex-devs)
        {::warning-type ::ex-authors
         ::authors (seq potential-ex-devs)
         ::threshold-months minexdevcontribtime}))))

;;
;; Risk Warnings
;;

(defn- early-warning-for-high-risk-commits
  [{:keys [time-now] :as context} project]
  (let [threshold (get-in project [:risk :warning-class] 7)
        days-back (get-in project [:risk :warning-days] 3)
        warning-destination paths/risk-by-commit-early-warnings-csv
        risk-warnings (risk-prediciton/predictions-matching context
                                                            {:risk-dest-file-name paths/risk-by-commit-csv
                                                             :warning-class threshold
                                                             :warning-days days-back}
                                                            time-now
                                                            warning-destination)]
    (when (seq risk-warnings)
      {::warning-type ::high-risk-commits
       ::number-of-risk-commits (count risk-warnings)})))

;;
;; Warnings for branches that are long-lived.
;;

(defn- n-ttl-warnings-for
  [warning-class ttls]
  (->> ttls (filter (partial = warning-class)) count))

(defn- early-warning-for-long-lived-branches
  [_context _project branches]
  (let [ttls (map :ttlwarning branches)
        red-warnings (n-ttl-warnings-for branch-statistics/red-ttl-warning ttls)
        yellow-warnings (n-ttl-warnings-for branch-statistics/yellow-ttl-warning ttls)]
    (when (or (< 0 red-warnings)
              (< 0 yellow-warnings))
      {::warning-type ::branch-long-ttl
       ::red-warnings red-warnings
       ::yellow-warnings yellow-warnings})))

;;
;; Warnings for supervised hotspots (notes).
;;

(defn note-specific-warnings-in
  [notes-risk]
  (let [warnings (->> notes-risk
                      (map :warnings)
                      (map :biomarkers-warning))
        warning-count (count (filter (partial = "warning") warnings))
        total-goal-count (count (filter #{"warning" "ok" "no-change"} warnings))]
    (when (pos? total-goal-count)
      {::warning-type ::notes-category-warning
       ::supervision-warnings warning-count
       ::total-goals total-goal-count})))

(defn notes-with-warnings-in
  [notes-risk]
  (let [biomarkers-warning #(get-in % [:warnings :biomarkers-warning])
        is-warning? #(= % "warning")
        note #(get-in % [:note-score :note])]
    (->> notes-risk
         (filter #(is-warning? (biomarkers-warning %)))
         (map note)
         (filter some?))))

(defn warn-for-supervised-hotspots
  [{:keys [analysis-path-fn]} _project]
  (let [notes-risk-file-path (analysis-path-fn paths/risks-for-notes-json)]
    (with-open [reader (clojure.java.io/reader notes-risk-file-path)]
      (note-specific-warnings-in (json-clj/read reader :key-fn keyword)))))

;;
;; Warnings for Brooks's Law
;;

; Avoid false positives since small orgs might fluctuate more:
(def ^:private minimum-authors-for-brooks 5)
(def ^:private monthly-decrease-for-development-output-warning-percentage -25)

(defn- has-monthtly-brooks-statistics?
  [{:keys [monthly-mean-authors monthly-authors-delta monthly-velocity-delta]}]
  (and monthly-mean-authors
       monthly-authors-delta
       monthly-velocity-delta))

(defn monthly-brooks-law-warning
  [{:keys [monthly-mean-authors monthly-authors-delta monthly-velocity-delta] :as s}]
  (when (and (has-monthtly-brooks-statistics? s)
             (>= monthly-mean-authors minimum-authors-for-brooks)
             (pos? monthly-authors-delta)
             (<= monthly-velocity-delta monthly-decrease-for-development-output-warning-percentage))
    {::warning-type ::monthly-brooks-law-category-warning
     ::author-increase monthly-authors-delta
     ::monthly-output-decrease monthly-velocity-delta}))

(defn monthly-development-output-warning
  [{:keys [monthly-velocity-delta] :as s}]
  (if (and (has-monthtly-brooks-statistics? s)
           (nil? (monthly-brooks-law-warning s)) ; Brooks's Law supercedes in importance
           (<= monthly-velocity-delta monthly-decrease-for-development-output-warning-percentage))
    {::warning-type ::monthly-development-output-category-warning
     ::monthly-output-decrease monthly-velocity-delta}))

(def ^:private weekly-author-delta-for-warning 2)

(defn- has-weekly-brooks-data?
  [{:keys [weekly-authors-delta weekly-velocity-delta]}]
  (and weekly-authors-delta
       weekly-velocity-delta))

(defn weekly-onboarding-warning
  [{:keys [weekly-authors-delta weekly-velocity-delta] :as s}]
  (if (and (has-weekly-brooks-data? s)
           (>= weekly-authors-delta weekly-author-delta-for-warning )
           (<= weekly-velocity-delta monthly-decrease-for-development-output-warning-percentage))
    {::warning-type ::weekly-onboarding-output-category-warning
     ::weekly-authors-delta weekly-authors-delta
     ::weekly-output-decrease weekly-velocity-delta}))

;;
;; Public API
;;

(defn early-warnings-json-path
  ([analysis-path]
   (early-warnings-json-path analysis-path (versioning/get-analysis-version analysis-path)))
  ([analysis-path analysis-version]
   (cond
     (> analysis-version 2)
     (io/file analysis-path paths/early-warnings-json)

     ;; Before version 3 no early warnings file was generated.
     :else
     nil)))

(defn- find-early-warnings
  [{:keys [analysis-path-fn] :as context} project branches]
  (let [development-output-statistics (json/parse-stream (io/reader (analysis-path-fn paths/development-output-json)) keyword)
        triggered-warnings [(generate-early-warnings-for-hotspots context)
                            (generate-early-warnings-for-complexity-trends context)
                            (generate-early-warnings-for-ex-developers context project)
                            (early-warning-for-high-risk-commits context project)
                            (early-warning-for-long-lived-branches context project branches)
                            (warn-for-supervised-hotspots context project)
                            (monthly-brooks-law-warning development-output-statistics)
                            (monthly-development-output-warning development-output-statistics)
                            (weekly-onboarding-warning development-output-statistics)]]
    (filter seq triggered-warnings)))

(s/fdef find-early-warnings
        :args (s/cat :context  (constantly true)
                     :project  ::specs/project
                     :branches ::branch-statistics/branch-statistics)
        :ret ::specs/analysis-warnings)

(defn generate-early-warnings
  "Runs an early warning analysis in case we have enough data."
  [context project branches]
  (-> (find-early-warnings context project branches)
      (json/generate-stream (io/writer (early-warnings-json-path (:analysis-path context)
                                                                 (:analysis-version context))))))
