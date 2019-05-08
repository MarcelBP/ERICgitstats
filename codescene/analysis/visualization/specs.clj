(ns codescene.analysis.visualization.specs
  (:require [clojure.spec.alpha :as s]
            [codescene.analysis.architecture.technical-sprawl :as sprawl]
            [codescene.analysis.specs :as shared-specs]))

;; This module contains the specs that are shared between the different clients of
;; the enclosure visualization such as file-level hotspots, architectural hotspots, etc.

(s/def ::revisions nat-int?)
(s/def ::code-size nat-int?)

(s/def ::hotspot (s/tuple ::shared-specs/filename ::revisions ::code-size))
(s/def ::hotspots (s/coll-of ::hotspot))

(s/def ::technical-sprawl (s/map-of ::shared-specs/filename ::sprawl/main-language))

(s/def ::ownership (s/and float? #(>= % 0.0) #(<= % 1.0)))
(s/def ::owner string?)
(s/def ::main-dev-knowledge (s/keys :req-un [::owner ::ownership]))
(s/def ::main-devs (s/map-of ::shared-specs/filename ::main-dev-knowledge))

(s/def ::age-in-month nat-int?)
(s/def ::code-age (s/map-of ::shared-specs/filename ::age-in-month))

(s/def ::loss ::ownership)
(s/def ::inconclusive boolean?)
(s/def ::loss-measure (s/keys :req-un [::loss ::inconclusive]))
(s/def ::knowledge-loss (s/map-of ::shared-specs/filename ::loss-measure))

(s/def ::relative-code-churn nat-int?)
(s/def ::churn (s/map-of ::shared-specs/filename ::relative-code-churn))