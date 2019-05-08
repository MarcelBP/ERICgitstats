(ns evolutionary-metrics.analysis.sum-of-coupling
  (:require [evolutionary-metrics.analysis.coupling-algos :as c]))

;;; This module calculates the sum of the temporal coupling for each module.
;;;
;;; The metric gives the number of shared transactions for a module.
;;; This gives you a priority list of the modules that are most
;;; frequently changed together with others.
;;;
;;; The analysis returns a dataset with the following columns:
;;;   :entity :soc
;;; where
;;;   :entity => the name of the module
;;;   :soc    => the sum of the coupling

(defn- entities-by-revision
  [ds]
  (map c/entities-in-rev
       (c/as-entities-by-revision ds)))

(defn- counted-entities
  [entities-in-rev]
  (let [n-couples (dec (count entities-in-rev))]
    (map (fn [e] [e n-couples]) entities-in-rev)))

(defn- changeset-filter-from
  [max-changeset-size]
  (if max-changeset-size
    (fn [commitset]
      (> (count commitset) max-changeset-size))
    (constantly false)))

(defn- entities-with-coupling-count-by-rev
  [max-changeset-size ds]
  (let [remove-changeset? (changeset-filter-from max-changeset-size)]
    (->> ds
         entities-by-revision
         (remove remove-changeset?)
         (mapcat counted-entities))))

(defn as-soc
  "Calculates a Sum of Coupling for each entity in 
   the dataset that passes the threshold for minimum 
   number of revisions."
  [ds {:keys [min-revs max-changeset-size]}]
  (->> ds
       (entities-with-coupling-count-by-rev max-changeset-size)
       (reduce (fn [acc [e n]]
                 (update-in acc [e] (fnil + 0) n))
               {})
       (into [])
       (filter (fn [[_e n]]
                 (>= n min-revs)))))

(defn by-degree
  "Calculates the sum of coupling. Returns a seq
   sorted in descending order (default) or an optional,
   custom sorting criterion."
  [ds options]
  (let [vs (->>
             (as-soc ds options)
             (sort-by (juxt second first))
             reverse)]
    {:analysis-result-header [:entity :soc]
     :analysis-result-values vs}))
