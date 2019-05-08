(ns evolutionary-metrics.trends.dates
  (:require [clj-time.format :as tf]
            [clj-time.core :as tc]))

(defn string->date
  [s]
  (tf/parse (tf/formatters :year-month-day) s))

(defn analysis-interval
  [s e]
  (tc/interval (string->date s)
               (string->date e)))

(defn date->string
  [d]
  (tf/unparse (tf/formatters :year-month-day) d))

(defn dates->strings
  [ds]
  (map date->string ds))

(defn date-time-string->date
  [s]
  (tf/parse (tf/formatters :date-time-no-ms) s))

(defn date-time->string
  [d]
  (tf/unparse (tf/formatters :date-time-no-ms) d))

