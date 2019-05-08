(ns codescene.presentation.health
    "For functions shared by system-health, hotspot-health and related
  namespaces."
  (:require
   [clojure.string :as string]
   [codescene.presentation.display :as display]
   [clojure.spec.alpha :as s]))


(defn val->status
  "Takes an integer, a min and a max and returns the corresponding status word (good, normal, bad)."
  [v min-val max-val]
  (cond
    (not (int? v)) "not-calculated"
    (<= v min-val) "bad"
    (and (< min-val v) (< v max-val)) "normal"
    (<= max-val v) "good"))

(defn zero->dash
  [score]
  (if (and (int? score) (zero? score))
    "-"
    score))


(s/def ::delivery-risk #{"green-stable"
                         "yellow-warning"
                         "red-warning"})
(s/def ::delivery-risk-val (s/or
                             :no-risk (complement int?)
                             :risk (s/and int? #(<= 0 % 10))))

(defn delivery-risk-val->status
  [v]
  (cond
    (not (int? v)) "green-stable"
    (<= v 6) "green-stable"
    (< 6 v 9) "yellow-warning"
    (<= 9 v) "red-warning"))
(s/fdef delivery-risk-val->status
  :args (s/cat :v ::delivery-risk-val)
  :ret ::delivery-risk)


(defn truncate-from-center
  "Replaces center of string with '...' to shorten string to less than
  `max-length`. If `max-length` is less than 20, 20 is used instead."
  [s max-length]
  (let [real-max-length (max max-length 20)]
    (if (>= real-max-length (count s))              
      s
      (let [s-length (count s)
            half-length-diff (max (int (/ (- s-length real-max-length) 2)) 1)
            split-point (int (/ s-length 2))
            first-part (subs s 0 (- split-point half-length-diff 3))
            second-part (subs s (+ split-point half-length-diff 3))]
        (str first-part "..." second-part)))))

(defn- replace-directory-by-offset
  [path max-length offset-from-center]
  (let [directories (string/split path #"/")
        center-idx (int (/ (count directories) 2))
        min-offset (- center-idx offset-from-center)
        max-offset (+ center-idx offset-from-center)]
    (if (and (pos? min-offset)
             (> (count directories) (inc max-offset))
             (> (count directories) 2))
      (let [dirs (->> directories
                      (map-indexed (fn [idx dir]
                                     (if (<= min-offset idx max-offset)
                                       nil
                                       dir)))
                      (partition-by nil?))]
        (string/join "/" (mapcat (fn [dir-group]
                                   (if (nil? (first dir-group))
                                     ["..."]
                                     dir-group))
                                 dirs)))

      (truncate-from-center path max-length))))        ; impossible, resort to brute force

(defn shorten-path
  "max-length must be at least 30. Smaller numbers default to 30."
  [path max-length]
  (let [real-max-length (max max-length 30)]
    (if (<= (count path) real-max-length)
      path
      (->> (range real-max-length)
           (map (partial replace-directory-by-offset path (int (/ (count path) 2))))
           (filter #(> real-max-length (count %)))
           first))))


(defn- code-health->code-health-statuses
  [{:keys [biomarkercurrent biomarkermonthback biomarkeryearback] :as code-health}]
  (assoc code-health
         :biomarkercurrent-status (val->status biomarkercurrent 3 8)
         :biomarkermonthback-status (val->status biomarkermonthback 3 8)
         :biomarkeryearback-status (val->status biomarkeryearback 3 8)))

(defn dashboard-parameters->code-health
  [dashboard-params]
  (let [code-health-params (->  dashboard-params
                                (update :biomarkercurrent (comp zero->dash display/as-int))
                                (update :biomarkermonthback (comp zero->dash display/as-int))
                                (update :biomarkeryearback (comp zero->dash display/as-int))
                                (select-keys [:biomarkercurrent :biomarkermonthback :biomarkeryearback]))]
    (if (some integer? ((juxt :biomarkercurrent :biomarkermonthback :biomarkeryearback) code-health-params))
      (code-health->code-health-statuses code-health-params)
      {})))

