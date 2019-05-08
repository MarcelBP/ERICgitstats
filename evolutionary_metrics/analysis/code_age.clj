(ns evolutionary-metrics.analysis.code-age
  (:require [evolutionary-metrics.analysis.internal.time :as time]
            [clj-time.core :as tc]))

;;; The following analysis is inspired by Dan North's presentation
;;; on keeping a short software half-life.
;;;
;;; The Code Age analysis is based on the idea that we want
;;; to have code that's either:
;;;  1) So old that it's a commodity stored away in stable libraries, or
;;;  2) Fresh in our minds so that we remember what it does.
;;;
;;; The algorithms in this module will calculate the age of each
;;; entity in months with respect to the last time the code was
;;; modified. It's then up to us to visualize it in a sensible way.

(defn- changes-within-time-span
  [changes now]
  (filter (comp (fn [date] (tc/before? (time/as-time date) now)) :date) changes))

(defn- latest-modification
  [changes]
  (->
   (map :date changes)
   sort
   last))

(defn- age-of-latest-in
  [changes now]
  (->
   (latest-modification changes)
   time/as-time
   (tc/interval now)
   tc/in-months))

(def has-content (complement empty?))

(defn- entities-by-latest-modification
  [now grouped]
  (for [[entity-entry changes] grouped
        :let [entity entity-entry
              relevant-changes (changes-within-time-span changes now)]
        :when (has-content relevant-changes)]
    {:entity entity
     :age-months (age-of-latest-in relevant-changes now)}))

(defn- move-only-modification?
  [{:keys [loc-added loc-deleted]}]
  (every? (partial = "0") [loc-added loc-deleted]))

(defn by-age
  [ds options]
  (let [vs (->>
             ds
             (remove move-only-modification?)
             (group-by :entity)
             (entities-by-latest-modification (time/time-now options))
             (sort-by :age-months <)
             (map (juxt :entity :age-months)))]
    {:analysis-result-header [:entity :age-months]
     :analysis-result-values vs}))
  
