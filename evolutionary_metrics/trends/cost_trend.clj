(ns evolutionary-metrics.trends.cost-trend
  (:require [evolutionary-metrics.trends.internal.pm-trends :as pm-trends]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [evolutionary-metrics.parsers.time-parser :as tp]
            [clj-time.core :as t]
            [clj-time.format :as tf]))

(defn- cost-of-revision
  [costs-by-id [rev date]]
  [rev date (get costs-by-id rev 0)])

(def ^:private date-field second)

(defn- cost-by-date
  [costs]
  (->> costs
       (partition-by date-field)
       (map (fn [costs-for-date]
              (let [date (-> costs-for-date first second)
                    c (->> costs-for-date (map #(nth % 2)) (reduce +))]
                [date c])))))

(def time-parser (tp/time-parser-from "yyyy-MM"))

(defn- as-time
  [time-as-string]
  (time-parser time-as-string))

(defn- slice-day
  "'date' should be a DateTime
  Returns a DateTime with days set to 1"
  [date]
  (t/date-time (t/year date) (t/month date) 1))

(defn- year-month-str
  [date]
  (tf/unparse (:year-month tf/formatters) date))

(defn- ensure-entry-for-current-date
  "We always want a trend that stretches until the
   current analysis date so that we can spot files where
   the costs suddenly stopped.

   'date-now' should be a DateTime"
  [date-now costs]
  (let [final-cost-field (last costs)
        current-day-month (slice-day date-now)]
    (if final-cost-field
      (if (t/before? (as-time (first final-cost-field)) current-day-month)
        (conj (vec costs) [(year-month-str current-day-month) 0])
        costs)
      []))) ; no cost

(defn trend-for-single-entity
  "'date-now' should be a DateTime"
  [commits-by-entity costs-by-id entity date-now]
  (->> entity
       (get commits-by-entity)
       (map (partial cost-of-revision costs-by-id))
       cost-by-date
       (ensure-entry-for-current-date date-now)))

(defn costs-trend-for
  "Generates a CSV file with the time series of cost trends for
   the given entities.
    - commits: a dataset with all commits
    - costs-by-id: a map with costs lookup by :rev
    - entitites: a seq of the file names we want to generate trends for.

    'date-now' should be a DateTime"
  [commits costs-by-id entities date-now outpath-root-name]
  (let [commits-by-entity (pm-trends/as-commits-by-entity commits (set entities))]
    (doseq [e entities]
      (let [cost-trend (trend-for-single-entity commits-by-entity costs-by-id e date-now)
            trend-file-name (.getPath (io/file outpath-root-name e "costtrend.csv"))]
        (when (seq cost-trend)
          (io/make-parents trend-file-name)
          (with-open [out-file (io/writer trend-file-name)]
            (csv/write-csv out-file [["date" "cost"]])
            (csv/write-csv out-file cost-trend)))))))
