(ns codescene.mining.cleaning)

(defn as-int
  [v]
  (if (integer? v)
    v
    (Integer/parseInt v)))

(defn normalize
  [max-val v]
  (float (/ (as-int v) max-val)))

(defn safe-max
  [vs]
  (if (empty? vs)
    0
    (apply max vs)))

(defn average [vs]
  (if (empty? vs)
    0
    (/ (reduce + vs) (count vs))))
