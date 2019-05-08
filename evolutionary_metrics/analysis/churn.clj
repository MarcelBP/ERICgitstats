(ns evolutionary-metrics.analysis.churn
  (:require [evolutionary-metrics.analysis.math :as math]
            [evolutionary-metrics.analysis.internal.churn-algos :as c]))

;;; This module contains functions for calculating churn metrics.
;;; Code churn is related to the quality of modules; the higher
;;; the churn, the more post-release defects.
;;; Further, inspecting the churn trend lets us spot certain
;;; organization-oriented patterns. For example, we may spot
;;; integration bottlenecks as spikes just before the end of
;;; one iteration.

(defn- as-ownership-ratio
  [own total result-conversion-fn]
  (->>
   (max total 1) ; some entities don't have any added lines (just removed)
   (/ own)
   result-conversion-fn))

(defn- grouped-by-author
  [changes]
  (group-by :author changes))

(defn- as-author-ownership
  "Calculates the percentage of ownership for each author
   in the given dataset for one (1) entity."
  [grouped total-added total-deleted result-conversion-fn]
  (for [[group-entry changes] grouped
        :let [author-name group-entry
              author-added (c/total-churn :loc-added changes)
              author-deleted (c/total-churn :loc-deleted changes)
              added-ownership (as-ownership-ratio author-added total-added result-conversion-fn)
              deleted-ownership (as-ownership-ratio author-deleted total-deleted result-conversion-fn)]]
    [author-name author-added added-ownership author-deleted deleted-ownership]))

(defn- sum-by-author-contrib
  [result-conversion-fn grouped]
  (for [[entity-entry changes] grouped
        :let [entity entity-entry
              total-added (c/total-churn :loc-added changes)
              total-deleted (c/total-churn :loc-deleted changes)
              author-group (grouped-by-author changes)
              author-contrib (as-author-ownership author-group total-added total-deleted result-conversion-fn)]]
    [entity author-contrib]))

(defn- normalize-contrib
  "Each entity is associated with its contributing authors.
   An author may be included multiple times. We need to
   join that info and splice one entity into multiple rows
   to make up an Incanter dataset.
   Example on input:
    [Entity [[ta 20 2] [at 2 0]]]
  Should result in:
    [Entity ta 20 2]
    [Entity at  2 0]"
  [[name contribs]]
  (map (fn [row] (into [name] row)) contribs))

(defn- churn-by
  [group ds]
  (->>
   (group-by group ds)
   c/sum-by-group
   (map (fn [g] (zipmap [group :added :deleted :net :revisions] g)))))

(defn absolutes-trend
  "Calculates the absolute code churn measures per date.
   Returns an Incanter dataset with the number of lines
   added and deleted each day (note that only dates wich
   involved commits are considered)."
  [commits _options]
  (let [vs (->> commits
                (churn-by :date)
                (sort-by (juxt :date :added :deleted))
                (map (juxt :date :added :deleted :net :revisions)))]
    {:analysis-result-header [:date :added :deleted :net :revisions]
     :analysis-result-values vs}))

(defn by-entity
  "Returns the absolute churn of each entity.
   The entities are sorted at churn rate in
   descending order based on the lines added
   metric. The idea is that entities
   with higher churn rate (even absolute) are
   more likely to contain post-release defects, where
   the number of lines added is a better predictor
   than lines deleted."
  [ds _options]
  (let [vs (->> ds
                (churn-by :entity)
                (sort-by :added)
                reverse
                (map (juxt :entity :added :deleted :net :revisions)))]
    {:analysis-result-header [:entity :added :deleted :net :revisions]
     :analysis-result-values vs}))

(defn- author-modifies-entity?
  "False if this is just a move operation, i.e. no code churn."
  [[_entity _author added _add-ownership deleted]]
  (or (pos? added)
      (pos? deleted)))

(defn as-ownership
  "Returns a table specifying the ownership of
   each module. Ownership is defined as the
   amount of churn contributed by each author
   to each entity."
  ([ds options]
   (as-ownership ds options float))
  ([ds _options result-conversion-fn]
   (let [vs (->>
              (group-by :entity ds)
              (sum-by-author-contrib result-conversion-fn)
              (mapcat normalize-contrib)
              (filter author-modifies-entity?)
              (sort-by (juxt first second)))]
     {:analysis-result-header [:entity :author :author-added :added-ownership :author-deleted :deleted-ownership]
      :analysis-result-values vs})))

;;
;;; Algorithms to identify main developers from churn
;;;

(defn- added-lines
  [[_author added _deleted]]
  added)

(defn- removed-lines
  [[_author _added _added-ownership deleted _deleted-ownership]]
  deleted)

(def developer first)

(defn- pick-main-developer
  "Picks the developer that contributed most lines of
   code (sure, a rough measure).
   Returns [Entity Developer Added Total-Added]
   Example on input:
    [Entity [[ta 20 2] [at 2 0]]]
  Should result in:
    [Entity ta 20 2]"
  ([author-contrib]
     (pick-main-developer added-lines author-contrib))
  ([metric-fn [name contribs]]
     (let [total-contrib (reduce + (map metric-fn contribs))
           main-dev (first (reverse (sort-by metric-fn contribs)))
           main-dev-contrib (metric-fn main-dev)
           ownership-ratio (as-ownership-ratio main-dev-contrib total-contrib math/ratio->centi-float-precision)]
       [name (developer main-dev) main-dev-contrib total-contrib ownership-ratio])))

(defn- grouped-by-author-contrib
  [ds]
  (sum-by-author-contrib
   math/ratio->centi-float-precision
   (group-by :entity ds)))


(defn by-main-developer
  "Identifies the main developer of each entity.
   The main developer is the one who has contributed
   most lines of code (default case).
   NOTE: see the alternative algorithm below!"
  [ds _options]
  (let [vs (->>
             (grouped-by-author-contrib ds)
             (map pick-main-developer)
             (sort-by (juxt :entity)))]
    {:analysis-result-header [:entity :main-dev :added :total-added :ownership]
     :analysis-result-values vs}))

(defn by-refactoring-main-developer
  "Identifies the main developer of each entity.
   The main developer in this alternative calculation
   is the developer that has _removed_ most lines.
   The idea/speculation is that when you remove code,
   you make a more active design choice than what can be
   expected from addition alone. We speak refactoring here."
  [ds _options]
  (let [vs (->>
             (grouped-by-author-contrib ds)
             (map (partial pick-main-developer removed-lines))
             (sort-by :entity))]
    {:analysis-result-header [:entity :main-dev :removed :total-removed :ownership]
     :analysis-result-values vs}))
