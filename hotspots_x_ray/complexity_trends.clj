(ns hotspots-x-ray.complexity-trends
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [evolutionary-metrics.trends.complexity-trend :as analysis-trends]
            [taoensso.timbre :as timbre]))

(defn trend-for
  [evolution function-name]
  (let [pts (->> evolution (mapcat :changes) (filter (comp (partial = function-name) :name)))]
    (if (seq pts)
      (->> pts
           analysis-trends/revisions-for-trend
           (map :complexity)
           (sort-by first))
      [])))

(defn write-trend-for
  [evolution function-name out-file-name]
  (with-open [out-file (io/writer out-file-name)]
    (let [complexity-trend (trend-for evolution function-name)]
      (timbre/trace "Generating trend for" function-name ":" (pr-str complexity-trend))
      (csv/write-csv out-file [["date" "total" "n" "mean" "median" "sd" "max" "comments"]])
      (csv/write-csv out-file complexity-trend))))
