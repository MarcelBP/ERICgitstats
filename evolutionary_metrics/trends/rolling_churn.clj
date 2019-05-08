;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.trends.rolling-churn
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as java-io]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.trends.rolling-average :as rolling]
            [evolutionary-metrics.trends.specs :as trends-specs]
            [evolutionary-metrics.core :as core]
            [incanter.core :as i]
            [incanter.zoo :as zoo]))

(defn as-zoo-ds
  [ds]
  (zoo/zoo ds :date))

(defn as-rolling-added-churn
  "Calculates a rolling average of the
   positive churn (added lines of code)."
  [ds n-days]
  (rolling/as-rolling-average-of ds n-days :added :rollingadded))

(defn as-rolling-deleted-churn
  "Calculates a rolling average of the
   negative churn (deleted lines of code)."
  [ds n-days]
  (rolling/as-rolling-average-of ds n-days :deleted :rollingdeleted))

(defn- as-modified-lines
  [added deleted]
  (max 0 (- added deleted)))

(defn as-rolling-modified-churn
  "Calculates a rolling average of the modification churn.
   Since we don't have modified lines of code in the original, we 
   try to deduce it here as a diff between added and removed."
  [ds n-days]
  (as-> ds res
       (i/$map as-modified-lines [:added :deleted] res)
       (i/dataset [:modified] res)
       (i/conj-cols ds res)
       (rolling/as-rolling-average-of res n-days :modified :rollingmodified)))

(defn- as-partial-ds
  [ds column]
  (as-> ds res
        (i/sel res :cols column)
        (rolling/fix-single-return-value-bug res)
        (map #(Math/round (float %)) res)
        (i/dataset [column] res)))

(defn as-multi-dimensional-churn
  [ds n-days]
  (let [added (as-partial-ds (as-rolling-added-churn ds n-days) :rollingadded)
        deleted (as-partial-ds (as-rolling-deleted-churn ds n-days) :rollingdeleted)
        modified (as-partial-ds (as-rolling-modified-churn ds n-days) :rollingmodified)]
    (i/conj-cols ds added deleted modified)))

;;
;; Raw CSV output
;;

(defn as-multi-dimensional-churn-trends
  [churn-file-name rolling-avg-days]
  (->
   (rolling/read-source-from churn-file-name)
   as-zoo-ds
   (as-multi-dimensional-churn rolling-avg-days)))

(s/fdef calculate-churn-trend
        :args (s/alt
                :std-out (s/cat :churn-file-name ::core/filename
                                :rolling-avg-days ::trends-specs/rolling-avg-days)
                :out-file (s/cat :churn-file-name ::core/filename
                                 :rolling-avg-days ::trends-specs/rolling-avg-days
                                 :out-file-name ::core/filename))
        :ret nil?)

(defn calculate-churn-trend
  "Generates a CSV file with the rolling churn."
  ([churn-file-name rolling-avg-days]
    (let [res (as-multi-dimensional-churn-trends churn-file-name rolling-avg-days)]
      (csv/write-csv *out* [(map name (i/col-names res))])
      (csv/write-csv *out* (i/to-list res))))
  ([churn-file-name rolling-avg-days out-file-name]
   (with-open [out-file (java-io/writer out-file-name)]
     (binding [*out* out-file]
       (calculate-churn-trend churn-file-name rolling-avg-days)))))
