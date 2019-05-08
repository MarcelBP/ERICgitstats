(ns evolutionary-metrics.trends.work-trend
  (:require [evolutionary-metrics.trends.internal.pm-trends :as pm-trends]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.core.matrix :as matrix]))

(def ^:private count-work-types (partial reduce +))

(defn- work-in-revision
  [costs-by-id work-by-id n-work-types [rev date]]
  (let [cost (get costs-by-id rev 0)
        work (get work-by-id rev (repeat n-work-types 0))
        n-types (count-work-types work)
        split-costs (/ cost (max n-types 1))
        cost-by-work (map (partial * split-costs) work)]
    (into [date] cost-by-work)))

(def ^:private date-field first)

(defn- work-by-date
  [work]
  (->> work
       (partition-by date-field)
       (map (fn [work-for-date]
              (let [date (->> work-for-date first date-field)
                    w (->> work-for-date (map #(drop 1 %)) (reduce matrix/add))]
                (into [date] w))))))

(defn trend-for-single-entity
  [commits-by-entity costs-by-id work-by-id n-work-types entity]
  (->> entity
       (get commits-by-entity)
       (map (partial work-in-revision costs-by-id work-by-id n-work-types))
       work-by-date))

(defn- trend-headers-for
  [supported-type-of-work]
  (into ["date"] supported-type-of-work))

(defn work-trends-for
  "Generates a CSV file with the time series of the reported type of work."
  [commits cost-by-id work-by-id supported-type-of-work entities outpath-root-name]
  {:pre [(map? cost-by-id) (map? work-by-id)]}
  (let [commits-by-entity (pm-trends/as-commits-by-entity commits (set entities))]
    (doseq [e entities]
      (let [work-trend (trend-for-single-entity commits-by-entity cost-by-id work-by-id (count supported-type-of-work) e)
            trend-file-name (.getPath (io/file outpath-root-name e "worktrend.csv"))]
        (when (seq work-trend)
          (io/make-parents trend-file-name)
          (with-open [out-file (io/writer trend-file-name)]
            (csv/write-csv out-file [(trend-headers-for supported-type-of-work)])
            (csv/write-csv out-file work-trend)))))))
