(ns codescene.mining.evolution-accessor
  (:require [evolutionary-metrics.analysis.internal.churn-algos :as churn]))

(defn revision [v]
  (:rev v))

(defn revision-date [v]
  (:date v))

(defn file-name
  [v]
  (:entity v))

(defn added-churn
  [v]
  (churn/as-int (:loc-added v)))

(defn deleted-churn
  [v]
  (churn/as-int (:loc-deleted v)))

(defn binary-content?
  [v]
  (some (partial = "-") [(:loc-added v) (:loc-deleted v)]))
