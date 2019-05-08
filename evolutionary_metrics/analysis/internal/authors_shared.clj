(ns evolutionary-metrics.analysis.internal.authors-shared
  (:require [clj-time.core :as tc]
            [evolutionary-metrics.parsers.time-parser :as tp]))

(defn last-contribution-for
  [author]
  (nth author 6)) ; NOTE: nasty, we depend upon a particular order!

(def time-parser (tp/time-parser-from "yyyy-MM-dd"))

(defn as-time
  [time-as-string]
  (time-parser time-as-string))

(def ^:private earliest-date first)

(def ^:private latest-date last)

(defn- contribution-time-span
  [changes]
  (let [dates (map :date changes)
        sorted-dates (sort dates)]
    [(earliest-date sorted-dates) (latest-date sorted-dates)]))

(defn contribution-time
  [single-author-commits]
  (let [[start-date last-date] (contribution-time-span single-author-commits)
        contribution-interval (tc/interval (as-time start-date) (as-time last-date))]
    [(tc/in-months contribution-interval) last-date start-date]))
