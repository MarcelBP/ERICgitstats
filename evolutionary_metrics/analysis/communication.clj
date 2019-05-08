(ns evolutionary-metrics.analysis.communication
  (:require [clojure.math.combinatorics :as combo]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.analysis.effort :as effort]
            [evolutionary-metrics.analysis.math :as m]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.parsers :as parsers]
            [clojure.math.numeric-tower :as math]
            ))

;;; This module attempts to give some heuristics on
;;; the communication needs of a project.
;;; The idea is basedo on Conway's law - a project
;;; works best when its organizational structure is
;;; mirrored in software.
;;;
;;; The algorithm is similiar to the one used for
;;; logical coupling: calculate the number of shared
;;; commits between all permutations of authors.
;;; Based on their total averaged commits, a
;;; communication strength value is calculated.

(s/def ::author   ::core/author-name)
(s/def ::peer     ::core/author-name)
(s/def ::shared   int?)
(s/def ::average  int?)
;; TODO juraj: can `strength` be ratio? How is it process then? (written as ratio to CSV file?)
(s/def ::strength (s/or :int int? :float float?))

(s/def ::communication-pair (s/keys :req-un [::author ::peer ::shared ::average ::strength]))

(s/def ::communication-by-shared-entities (s/coll-of ::communication-pair))

(defn- authors-of
  [changes]
  (distinct
   (map :author changes)))

;;; When calculating frequencies we get all permutations,
;;; including "noise" like self-self pairs. We use that
;;; noise to carry information for us - self-self will
;;; give us a fast way to look-up the total number of
;;; commits for an author.

(defn- authorship-combos
  [authors]
  (combo/selections authors 2))

(defn- entity-group->authorship-combos
  [[_entity-entry changes]]
  (authorship-combos (authors-of changes)))

(defn- author-pairs-for-entities
  "Transforms the given dataset (grouped on entity) into
   a seq of author pairs. The frequency of each pair in
   the returned seq will specify their amount of shared
   work over the grouped entities."
  [grouped]
  (mapcat entity-group->authorship-combos grouped))

(defn- by-shared-work-frequency
  [authors-by-work]
  (frequencies authors-by-work))

(defn- commits-of
  [author freqs]
  (freqs [author author]))

(defn- strength-from
  [shared-commits average-commits]
  (int
   (m/as-percentage
    (/ shared-commits average-commits))))

(defn- distinct-pairs
  [freqs]
  (->> freqs
       (map (fn [[pair shared-commits]] [(sort pair) shared-commits]))
       distinct))

(s/fdef with-commit-stats
        :ret ::communication-by-shared-entities)

(defn- with-commit-stats
  "The statistics are calculated from the raw
   data, freqs. The data contains pairs for all
   authors with their shared work frequencies.
   The statistics (i.e. total number of commits) for
   any author is retrieved by looking-up the
   value for the author paired with himself.
   That self-pairing is stripped from the final
   statistics but used here to carry information."
  [{:keys [social-net-min-shared-revs] :or {social-net-min-shared-revs 20}} freqs]
  (for [[pair shared-commits] (distinct-pairs freqs)
       :let [[me peer] pair
             my-commits (commits-of me freqs)
             peer-commits (commits-of peer freqs)
             average-commits (int (math/ceil (m/average my-commits peer-commits)))
             strength (strength-from shared-commits average-commits)]
       :when (and (not (= me peer))
                 (>= shared-commits social-net-min-shared-revs))]
    {:author me :peer peer :shared shared-commits :average average-commits :strength strength}))

(s/def ::max-changeset-size int?)

(s/fdef filter-large-changesets
        :args (s/cat :ds ::parsers/vcs-log
                     :options (s/keys :opt-un [::max-changeset-size]))
        :ret ::parsers/vcs-log)
(defn- filter-large-changesets
  [ds {:keys [max-changeset-size] :or {max-changeset-size 50}}]
  (->> ds
       (group-by :rev)
       (remove (fn [[_k changes]] (> (count changes) max-changeset-size)))
       (map second)
       (apply concat)))

(s/fdef by-shared-entities
        :args (s/cat :ds ::parsers/vcs-log
                     :options (s/keys :opt-un [::max-changeset-size
                                               ::social-net-min-shared-revs]))
        :ret ::core/analysis-result)
(defn by-shared-entities
  "Caclulates the communication needs as based upon
   shared work by the authors on different entities.
   Returns a dataset containing pairs of all permutations
   of authors with a (heuristic) communication strength
   value for each pair."
  [ds options]
  (let [vs (as-> ds $
                 (filter-large-changesets $ options)
                 (effort/as-revisions-per-author $ options)
                 (map (partial zipmap (:analysis-result-header $)) (:analysis-result-values $))
                 (group-by :entity $)
                 (author-pairs-for-entities $)
                 (by-shared-work-frequency $)
                 (with-commit-stats options $)
                 (sort-by (juxt :strength :author) $)
                 (map (juxt :author :peer :shared :average :strength) $)
                 (reverse $))]
    {:analysis-result-header [:author :peer :shared :average :strength]
     :analysis-result-values vs}))
