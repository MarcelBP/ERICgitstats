(ns hotspots-x-ray.recommendations.proximity
  (:require [clojure.spec.alpha :as s]
            [hotspots-x-ray.recommendations.specs :as recommendations-spec]
            [hotspots-x-ray.languages.specs :as languages-specs]
            [hotspots-x-ray.analysis-results-spec :as analysis-results-spec]))

;;; The proximity principle focuses on how well organized your code is with respect to readability and change.
;;; In general, you want to keep functions that are changed together in close proximity in the source code.
;;; You use proximity both as a design principle and as a heuristic to evaluate the cohesion and structure of existing code.
;;;
;;; This module calculates the proximity between all temporal clusters. We recommend a move refactoring as soon as
;;; there's a distance between the parts in a cluster.
;;;
;;; NOTE that we only analyse functions _within_ a single file.
;;;
;;; NOTE that proximity is measured by the number of functions between each couple; Lines of Code isn't considered
;;; in the current version.

(defn- indexed-fn-stats-by-name
  "Returns a map of <name> => <relative function position> within the file."
  [fns]
  (->> fns
       (map (juxt :name :start-line))
       (sort-by :start-line)
       (map first)
       (map-indexed (comp reverse vector))
       (map (partial into []))
       (into {})))

(defn- coupling-by-entity
  [r]
  (->> r
       (map (juxt :entity :coupled))
       (map sort)
       (reduce (fn [acc [p1 p2]]
                 ((fnil #(assoc acc p1 (conj % p2)) #{}) (get acc p1)))
               {})))

(defn- find-indirect-couples-for
  [all-couples direct-couples]
  {:post [#(set? %)]}
  (->> direct-couples
       (map (partial get all-couples))
       (apply concat)
       (into #{})))

(defn- cluster-for
  [couples entity]
  {:pre [(map? couples) (string? entity)]}
  (let [direct-couples (get couples entity)
        indirect-couples (find-indirect-couples-for couples direct-couples)]
    (into direct-couples indirect-couples)))

(def ^:private pair-fns (partial partition 2 1))

(defn- fn-distance
  [[f1 f2]]
  (let [[f1-name f1-pos] f1
        [f2-name f2-pos] f2]
    [(- f2-pos f1-pos 1) f1-name f2-name]))

(defn- limit-depth-by-threshold
  [{:keys [proximity-max-depth] :or {proximity-max-depth 4}} deviations]
  (->> deviations
       (sort-by first >)
       (take proximity-max-depth)))

(defn- proximity-deviations-for
  [entity dependants fn-positions thresholds]
  (->> #{entity}
       (into dependants)
       (map (juxt identity fn-positions))
       (sort-by second)
       pair-fns
       (map fn-distance)
       (remove (comp (partial = 0) first))
       (limit-depth-by-threshold thresholds)))

(defn- cluster-presentation-format
  [deviations]
  (let [total-proximity (->> deviations (map first) (reduce +))
        entities (->> deviations (map rest) flatten distinct)]
    {:entities entities :totalproximity total-proximity}))

(defn- pair-presentation-format
  [deviations]
  (->> deviations
       distinct
       (map (fn [[p e1 e2]] {:totalproximity p :entity1 e1 :entity2 e2}))))

(defn- proximity-deviations
  [lookupable fn-positions thresholds]
  (for [entity (keys lookupable)
        :let [dependendants (cluster-for lookupable entity)
              deviations (proximity-deviations-for entity dependendants fn-positions thresholds)]
        :when (seq deviations)]
    deviations))

(defn- part-of-longer-chain?
  [lookupable {:keys [entities]}]
  (let [comparable-entities (set entities)]
    (some (fn [s]
            (and (not= comparable-entities s)
                 (clojure.set/subset? comparable-entities s)))
          lookupable)))

(defn- filter-sub-recommendations
  [recommendations]
  (let [lookupable (->> recommendations (map :entities) (map (partial into #{})))]
    (->> recommendations
         distinct
         (remove (partial part-of-longer-chain? lookupable)))))

(defn- limit-results
  [{:keys [proximity-max-results] :or {proximity-max-results 10}} recommendations]
  (take proximity-max-results recommendations))

(defn recommend-refactorings
  "Returns a sequence of recommended proximity refactorings.
   Each recommendation specifies two, or more, functions that
   should be moved closer to each other for increased readability."
  ([current-version temporal-coupling-results]
   (recommend-refactorings current-version temporal-coupling-results {}))
  ([current-version temporal-coupling-results thresholds]
   {:pre [(s/valid? ::languages-specs/parsed-function-statistics current-version)
          (s/valid? ::analysis-results-spec/temporal-coupling-result temporal-coupling-results)]
    :post [(s/valid? ::recommendations-spec/proximity-pair-result %)]}
   (let [lookupable (coupling-by-entity temporal-coupling-results)
         fn-positions (indexed-fn-stats-by-name current-version)]
     (->> (proximity-deviations lookupable fn-positions thresholds)
          (apply concat)
          pair-presentation-format
          (sort-by :totalproximity >)
          (limit-results thresholds)))))

(defn recommend-cluster-refactorings
  "Returns a sequence of recommended cluster proximity refactorings.
   Each recommendation specifies a cluster functions that
   should be moved closer to each other for increased readability.
   This cluster version of the algorithm is intended for external temporal
   coupling clusters."
  ([current-version temporal-coupling-results]
   (recommend-cluster-refactorings current-version temporal-coupling-results {}))
  ([current-version temporal-coupling-results thresholds]
   {:pre [(s/valid? ::languages-specs/parsed-function-statistics current-version)
          (s/valid? ::analysis-results-spec/temporal-coupling-result temporal-coupling-results)]
    :post [(s/valid? ::recommendations-spec/proximity-cluster-result %)]}
   (let [lookupable (coupling-by-entity temporal-coupling-results)
         fn-positions (indexed-fn-stats-by-name current-version)]
     (->> (proximity-deviations lookupable fn-positions thresholds)
          (map cluster-presentation-format)
          filter-sub-recommendations
          (sort-by :totalproximity >)))))
