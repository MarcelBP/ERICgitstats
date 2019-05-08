;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.trends.rolling-average
  (:require [incanter.core :as i]
            [incanter.zoo :as zoo]
            [incanter.io :as io]
            [evolutionary-metrics.analysis.math :as math]))

(defn read-source-from
  [file-name]
  (io/read-dataset file-name :header true))

(defn fix-single-return-value-bug
  "Workaround for what seems to be a flaw in Incanter.
   When returning a single value, that value is returned,
   not a seq."
  [r]
  (if (seq? r) r [r]))

(defn as-rolling-average-of
  "Calculates a rolling average of the
   given column, which is added as a 
   target column to the returned dataset."
  [ds n-days churn-column target-column]
  (->>
   (i/sel ds :cols churn-column)
   fix-single-return-value-bug
   (zoo/roll-mean n-days)
   (map (partial math/round-with-precision 2))
   (i/dataset [target-column])
   (i/conj-cols ds)))
