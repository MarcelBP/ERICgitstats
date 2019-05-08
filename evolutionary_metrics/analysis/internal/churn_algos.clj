(ns evolutionary-metrics.analysis.internal.churn-algos
  (:require [medley.core :as medley]))

;;; This module contains shared functionality for calculating churn metrics.

(defn as-int
  "Binaries are given as a dash.
   Ensure these are replaced by zeros."
  [v]
  (Integer/parseInt
    (if (= "-" v) "0" v)))

(defn total-churn
  [selector ds]
  (reduce +
          (map as-int (map selector ds))))

(defn count-distinct-in
  [field ds]
  (->> ds
       (medley/distinct-by field)
       count))

(defn revisions-in
  [ds]
  (count-distinct-in :rev ds))

(defn statistics-for-group-entry
  [group-entry changes]
  (let [grouping group-entry
        added (total-churn :loc-added changes)
        deleted (total-churn :loc-deleted changes)
        net (- added deleted)
        revs (revisions-in changes)]
    [grouping added deleted net revs]))

(defn sum-by-group
  "Sums the given dataset by a given group and churn.
   The given dataset, grouped-ds, is grouped by the column
   given as group.
   That means, each entry is a pair of some grouping construct
   and the changes related to that construct. The changes are
   Incanter datasets themselves so we can keep using
   Incanter to extract data for each group."
  [grouped]
  (map (fn [[group-entry changes]] (statistics-for-group-entry group-entry changes)) grouped))