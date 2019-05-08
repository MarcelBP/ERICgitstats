;;; Copyright (C) 2013 Adam Tornhill, 2015 Empear

(ns evolutionary-metrics.analysis.effort
  (:require [clojure.math.numeric-tower :as m]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.analysis.math :as math]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.analysis.internal.churn-algos :as algos]
            [evolutionary-metrics.parsers :as parsers]))

;;; The idea behind effort is to identify how much each author
;;; contributed to a module. The measure here is a bit more
;;; rough than the churn metrics. On the other hand, the metric
;;; is available for all supported VCS.
;;; I use the generated statistics as a guide when refactoring;
;;; by ranking the authors based on their amount of contribution
;;; I know who to ask when visiting a new module.
;;;
;;; The analysis in the module is based on research by
;;; Marco D’Ambros, Harald C. Gall, Michele Lanza, and Martin Pinzger.

(defn normalize-effort
  [[name effort]]
  (map (fn [[author revs total-revs]]
         [name author revs total-revs])
       effort))

(defn- sum-revs-by-author
  "Sums the given dataset by a given group and churn.
   The given dataset, grouped-ds, is grouped by the column
   given as group.
   That means, each entry is a pair of some grouping construct
   and the changes related to that construct. The changes are
   Incanter datasets themselves so we can keep using
   Incanter to extract data for each group."
  [grouped total-revs]
  (for [[group-entry changes] grouped
        :let [author group-entry
              revs (->> changes (map :rev) distinct count)]]
    [author revs total-revs]))

(defn sum-effort-by-author
  [grouped]
  (for [[entity-entry changes] grouped
        :let [entity entity-entry
              total-revs (->> changes (map :rev) distinct count)
              author-group (group-by :author changes)
              author-revs (sum-revs-by-author author-group total-revs)]]
    [entity author-revs]))

(s/fdef as-revisions-per-author
        :args (s/cat :ds ::parsers/vcs-log
                     :options map?)
        :ret  ::core/analysis-result)
(defn as-revisions-per-author
  [ds _options]
  (let [vs (->>
             (group-by :entity ds)
             sum-effort-by-author
             (mapcat normalize-effort)
             (sort-by first))]
    {:analysis-result-header [:entity :author :author-revs :total-revs]
     :analysis-result-values vs}))

(defn- contributed-revs
  [author-changes]
  (let [[_author added _total] author-changes]
    added))

(defn- pick-main-dev-by-rev
  [entity-ds]
  (let [[entity entity-changes] entity-ds
        main-dev-changes (first (sort-by contributed-revs > entity-changes))
        [author added total] main-dev-changes
        ownership (math/ratio->centi-float-precision (/ added total))]
    {:entity entity
     :author author
     :added added
     :total total
     :ownership ownership}))

(s/fdef as-main-developer-by-revisions
        :args (s/cat :ds ::parsers/vcs-log
                     :options map?)
        :ret  ::core/analysis-result)
(defn as-main-developer-by-revisions
  "Identifies the main developers, together with their
   ownership percentage, of each module."
  [ds options]
  (let [vs (->> ds
                (group-by :entity)
                sum-effort-by-author
                (map pick-main-dev-by-rev)
                (sort-by :entity)
                (map (juxt :entity :author :added :total :ownership)))]
    {:analysis-result-header [:entity :main-dev :added :total-added :ownership]
     :analysis-result-values vs}))

(defn- as-author-fractals
  [[_ ai nc]]
  (m/expt (/ ai nc) 2))

(defn- as-fractal-value
  [[name effort]]
  (let [author-efforts (first effort) ; same as nc
        [_1 _2 total-revs] author-efforts
        fv1 (reduce + (map as-author-fractals effort))
        fv (math/ratio->centi-float-precision (- 1 fv1))
        n-authors (count effort)] ; one vector per author
    {:name name
     :fractal-value fv
     :total-revs total-revs
     :n-authors n-authors}))

(defn- modifying-commits-in
  [ds]
  (filter (fn [{:keys [loc-added loc-deleted]}]
            (or (not= "0" loc-added)
                (not= "0" loc-deleted)))
          ds))

(defn- sum-locs-by-author
  [grouped total-loc]
  (for [[group-entry changes] grouped
        :let [author group-entry
              locs (->> changes (map :loc-added) (reduce +))]]
    [author locs total-loc]))

(defn- loc-effort-by-author
  [grouped]
  (for [[entity-entry changes] grouped
        :let [entity entity-entry
              total-added-locs (->> changes (map :loc-added) (reduce +))
              author-group (group-by :author changes)
              author-locs (sum-locs-by-author author-group total-added-locs)]
        :when (< 0 total-added-locs)]
    [entity author-locs]))

(defn as-entity-fragmentation
  "Caclulates a fractal value for each entity.
   The fractal value ranges from 0 (one author) to
   1 (many authors, unreachable value).
   The fractal value is a good complement to number of
   authors analyses since here we reduce smaller contributions
   more and get a chance to find the truly fragmented entities."
  [ds _options]
  (let [vs (->>
             (modifying-commits-in ds)
             (map (fn [{:keys [loc-added loc-deleted] :as c}]
                    (-> c
                        (assoc :loc-added (algos/as-int loc-added))
                        (assoc :loc-deleted (algos/as-int loc-deleted)))))
             (group-by :entity)
             loc-effort-by-author
             (map as-fractal-value)
             (sort-by (juxt :fractal-value :n-authors :total-revs))
             (map (juxt :name :fractal-value :total-revs :n-authors))
             reverse)]
    {:analysis-result-header [:entity :fractal-value :total-loc :n-authors]
     :analysis-result-values vs}))

(defn as-revisions-based-entity-fragmentation
  "Caclulates a fractal value for each entity.
   The fractal value ranges from 0 (one author) to
   1 (many authors, unreachable value).
   The fractal value is a good complement to number of
   authors analyses since here we reduce smaller contributions
   more and get a chance to find the truly fragmented entities."
  [ds options]
  (let [vs (->>
             (modifying-commits-in ds)
             (group-by :entity)
             sum-effort-by-author
             (map as-fractal-value)
             (sort-by (juxt :fractal-value :n-authors :total-revs))
             (map (juxt :name :fractal-value :total-revs :n-authors))
             reverse)]
    {:analysis-result-header [:entity :fractal-value :total-revs :n-authors]
     :analysis-result-values vs}))
