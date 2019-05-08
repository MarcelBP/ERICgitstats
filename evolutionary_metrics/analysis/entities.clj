(ns evolutionary-metrics.analysis.entities
  (:require [medley.core :as medley]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.parsers :as parsers]))

(defn all [commits]
  (medley/distinct-by :entity commits))

(defn- group->entity-with-rev-count
  [[entity-group changes]]
  {:entity entity-group
   :n-revs (count (distinct (map :rev changes)))})

(defn all-revisions
  [commits]
  (medley/distinct-by :rev commits))

(defn as-dataset-by-revision
  [ds]
  (->>
    ds
    (group-by :entity)
    (map group->entity-with-rev-count)
    (sort-by :n-revs)
    reverse
    (map (juxt :entity :n-revs))))

(s/fdef by-revision
        :args (s/cat :ds ::parsers/vcs-log
                     :options map?)
        :ret ::core/analysis-result)
(defn by-revision
  "Sorts all entities in the dataset ds by
   their number of revisions."
  [ds _options]
  {:analysis-result-header [:entity :n-revs]
   :analysis-result-values (as-dataset-by-revision ds)})
  