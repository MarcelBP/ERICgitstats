(ns evolutionary-metrics.analysis.coupling-algos
  (:require [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]
            [clojure.spec.alpha :as s]))

;;; This module contains the shared algorithms for the
;;; different coupling measures.

;;; coupling algos

(s/def ::minrevs nat-int?)
(s/def ::minsharedrevs nat-int?)
(s/def ::mincoupling nat-int?)
(s/def ::maxcoupling nat-int?)
(s/def ::maxchangesetsize nat-int?)

(defn- drop-duplicates
  [entities]
  (remove #(= % (reverse %)) entities))

(defn- drop-mirrored-modules
  "Removed mirrored change sets such as:
    [A B] [B A] => [A B]"
  [entities]
  (distinct (map sort entities)))

(defn- as-co-changing-modules
  "Returns pairs representing the modules
   coupled in the given change set.
   Note that we keep single modules that
   aren't coupled - we need them to calculate
   the correct number of total revisions."
  [entities]
  (drop-mirrored-modules (combo/selections entities 2)))

(defn as-entities-by-revision
  "Extracts the change set per revision
   from an Incanter dataset."
  [ds]
  (->> ds
       (map (fn [m] (select-keys m [:rev :entity]))) ; minimal dataset to save memory footprint
       (group-by :rev)
       (map second)))

(defn within-threshold?
  "Used to filter the results based on user options."
  [{:keys [min-revs min-shared-revs min-coupling max-coupling]}
   revs shared-revs coupling]
  {:pre [(s/valid? ::minrevs min-revs)
         (s/valid? ::minsharedrevs min-shared-revs)
         (s/valid? ::mincoupling min-coupling)
         (s/valid? ::maxcoupling max-coupling)]}
  (and
   (>= revs min-revs)
   (>= shared-revs min-shared-revs)
   (>= coupling min-coupling)
   (<= (math/floor coupling) max-coupling)))

(def entities-in-rev
  (partial map :entity))

(def modules-in-one-rev
  "We receive pairs of co-changing modules in a
   revision  and return a seq of all distinct modules."
  (comp distinct flatten))

(defn module-by-revs
  "Returns a map with each module as key and
   its number of revisions as value.
   This is used when calculating the degree
   of coupling later."
  [all-co-changing]
  (frequencies (mapcat modules-in-one-rev all-co-changing)))

(defn exceeds-max-changeset-size?
  [max-size change-set]
  {:pre [(s/valid? ::maxchangesetsize max-size)]}
  (> (count change-set) max-size))

(defn co-changing-by-revision
  "Calculates a vector of all entities coupled
  in the revision represented by the dataset."
  [ds options]
  (->>
   (as-entities-by-revision ds)
   (map entities-in-rev)
   (remove (partial exceeds-max-changeset-size? (:max-changeset-size options)))
   (map as-co-changing-modules)))

(defn coupling-frequencies
  "Returns a map with pairs of coupled
   modules (pairs) as keyes and their
   number of shared revisions as value."
  [co-changing]
  (->
   (apply concat co-changing)
   drop-duplicates ;; remember: included to get the right total revisions
   frequencies
   vec))
