(ns hotspots-x-ray.recommendations.specs
  (:require [clojure.spec.alpha :as s]))

;; Proximity results
;;

(s/def ::entity string?)
(s/def ::entities (s/coll-of ::entity))

(s/def ::totalproximity integer?)

(s/def ::entity1 ::entity)
(s/def ::entity2 ::entity)

(s/def ::proximity-pair (s/and (s/keys :req-un [::entity1 ::entity2 ::totalproximity])
                               #(not= (:entity1 %) (:entity2 %))))

(s/def ::proximity-pair-result (s/coll-of ::proximity-pair))

;; A proximity cluster is a seq of several related functions in, possibly, multiple files.
;;

(s/def ::proximity-cluster (s/keys :req-un [::entities ::totalproximity]))

(s/def ::proximity-cluster-result (s/coll-of ::proximity-cluster))

