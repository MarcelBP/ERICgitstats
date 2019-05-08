(ns codescene.analysis.conversions
  (:require [clojure.spec.alpha :as s]))

(defn as-fraction
  [v]
  {:pre [(s/valid? number? v)]}
  (float (/ v 100.0)))

(defn as-fraction-str
  [v]
  (str (as-fraction v)))
