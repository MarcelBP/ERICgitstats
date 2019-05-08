(ns hotspots-x-ray.recommendations.code-health.size-detectors)

(defn few-functions?
  [ths {:keys [n-functions]}]
  (< n-functions (:few-functions ths)))
