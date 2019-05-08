(ns hotspots-x-ray.analysis-results-spec
  (:require [clojure.spec.alpha :as s]))

;;; NOTE: all these types shall be moved to closed-maat and the respective analysis!


;; Temporal coupling
;;

(s/def ::entity string?)
(s/def ::coupled string?)
(s/def ::degree integer?)
(s/def ::average-revs integer?)
(s/def ::similarity integer?)

(s/def ::temporal-coupling (s/keys :req-un [::entity ::coupled ::degree ::average-revs ::similarity]))

(s/def ::temporal-coupling-result (s/coll-of ::temporal-coupling))
