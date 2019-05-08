;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.trends.code-growth
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as java-io]
            [evolutionary-metrics.mergers.shared-mergers :as shared]))

(defn- current-code-size
  [summary-file]
  (->> (shared/read-csv-sans-header-from summary-file)
       (map (fn [[_language _filename blank comment code]]
              (+ (Integer/parseInt blank)
                 (Integer/parseInt comment)
                 (Integer/parseInt code))))
       (reduce +)))

(defn as-delta-churn-by-date
  [churn-trend]
  (map (fn [[date added deleted]]
         [date (- (Integer/parseInt added) (Integer/parseInt deleted))])
       churn-trend))

(defn as-delta-in-following-commit
  [growth-from-end-by-date]
  (->> growth-from-end-by-date
       (map second)
       (into [0])))

(defn as-growth-with-delta
  [growth-from-end-by-date]
  (let [delta (as-delta-in-following-commit growth-from-end-by-date)]
    (map (fn [g d] (into g [(- d)])) growth-from-end-by-date delta)))

(defn as-rolling-loc-growth
  "Calculates the accumulation of code over time.
   The calculation is done in reverse since our final LoC size is
   the only known point (we may have started in the middle of the
   evolution of a codebase."
  [churn-trend final-loc-size]
  (let [delta-churn-by-date (as-delta-churn-by-date churn-trend)
        growth-from-end (reverse delta-churn-by-date)
        growth-with-delta (as-growth-with-delta growth-from-end)]
    (->> growth-with-delta
         (reduce (fn [acc [date growth delta]]
                   (let [[rolling-total res] acc
                         new-total (+ rolling-total delta)]
                     [new-total (into res [[date new-total growth]])]))
                 [final-loc-size []])
         second
         reverse)))

(defn calculate-code-growth
  "Generates a CSV file with a time series specifying accumulation of Lines of Code."
  ([churn-trend-file-name loc-summary-file-name]
    (let [final-loc-size (current-code-size loc-summary-file-name)
          churn-trend (shared/read-csv-sans-header-from churn-trend-file-name)
          res (as-rolling-loc-growth churn-trend final-loc-size)]
      (csv/write-csv *out* [["index" "size" "growth"]])
      (csv/write-csv *out* res)))
  ([churn-trend-file-name loc-summary-file-name out-file-name]
   (with-open [out-file (java-io/writer out-file-name)]
     (binding [*out* out-file]
       (calculate-code-growth churn-trend-file-name loc-summary-file-name)))))
