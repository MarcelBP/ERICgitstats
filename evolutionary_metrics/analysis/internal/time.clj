(ns evolutionary-metrics.analysis.internal.time
  (:require [clj-time.core :as tc]
            [evolutionary-metrics.parsers.time-parser :as tp]))

(def time-parser (tp/time-parser-from "yyyy-MM-dd"))

(defn as-time
  [time-as-string]
  (time-parser time-as-string))

(defn time-now
  [options]
  (if-let [given-time (:age-time-now options)]
    (as-time given-time)
    (tc/now)))
