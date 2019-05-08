(ns hotspots-x-ray.languages.specs
  (:require [clojure.spec.alpha :as s]))

;; Domain types for parsed entities
;;

(s/def ::name string?)
(s/def ::start-line integer?)
(s/def ::end-line integer?)

(s/def ::function-statistic (s/and (s/keys :req-un [::name ::start-line ::end-line])
                                   #(>= (:end-line %) (:start-line %))))

(s/def ::parsed-function-statistics (s/coll-of ::function-statistic))

