(ns hotspots-x-ray.recommendations.code-statistics)

(defn loc-from
  [{:keys [start-line end-line] :as _function}]
  (inc (- end-line start-line)))
