(ns hotspots-x-ray.recommendations.code-biomarkers-trend-detectors
  (:require [hotspots-x-ray.recommendations.code-biomarkers-spec :as biomarkers-spec]
            [clojure.spec.alpha :as s]
            [taoensso.timbre :as log]))

(defn no-historic-trend-score
  "Intended as a stub in the case where we aren't interested in detecting
   historic trends. This could be for samples that a far into the past (last year) or
   maybe some test cases."
  [_current-scores]
  {})

(defn- nested-delta-from
  [historic {:keys [max-nested-complexity-depth]}]
  (- max-nested-complexity-depth (:max-nested-complexity-depth historic)))

(defn historic-trends-from
  [historic-scores {:keys [active-code-size cc-total nested n-functions] :as current-scores}]
  (let [historic-details (:details historic-scores)]
    (if (s/valid? ::biomarkers-spec/code-properties historic-details)
      (do
        (log/debug "Scoring historic trends for biomarkers")
        {:delta {:active-code-delta (- active-code-size (:active-code-size historic-details))
                 :cc-total-delta (- cc-total (:cc-total historic-details))
                 :nested-depth-delta (nested-delta-from (:nested historic-details) nested)
                 :n-functions-delta (- n-functions (:n-functions historic-details))}})
      (no-historic-trend-score current-scores))))
