(ns codescene.analysis.pm-management-specs
  (:require [clojure.spec.alpha :as s]
            [evolutionary-metrics.core]
            [evolutionary-metrics.trends.specs]))


;; PM Integration
;;

;; Specs for the PM data source (our input):
(s/def ::id string?)
(s/def ::idType string?)
(s/def ::type string?)
(s/def ::costUnit (s/keys :req-un [::type]))
(s/def ::workTypes (s/coll-of string?))

(s/def ::project-items (s/keys :req-un [::id ::idType ::costUnit ::workTypes]))

(s/def ::pm-projects (s/coll-of ::project-items))

;; Specs for the PM cost raw data:
(s/def ::id-type  integer?)
(s/def ::cost-unit ::costUnit)
(s/def ::cost-item (s/cat :id string? :cost integer?))
(s/def ::items (s/coll-of ::cost-item))

(s/def ::cost-by-item (s/keys :req-un [::id-type ::cost-unit ::items]))

(s/def ::supported-types  (s/coll-of string?))

(s/def ::binary-value (s/and integer? #(or (= 1  %) (zero? %))))
(s/def ::single-item-type (s/cat :id string? :types (s/* ::binary-value)))
(s/def ::items-with-types (s/coll-of ::single-item-type))

(defn- item-spec-matches-supported-types?
  [{:keys [items-with-types supported-types]}]
  (if (empty? items-with-types)
    true
    (let [sample (first items-with-types)]
      (= (count (:types sample)) (count supported-types)))))

(s/def ::type-of-work-items (s/and (s/keys :req-un [::id-type ::supported-types ::items-with-types])
                                   item-spec-matches-supported-types?))
