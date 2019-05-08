(ns evolutionary-metrics.mergers.dependency-matrix
  (:require [clojure.data.json :as json]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(defn matrix-from
  "Receives a seq of rows with [p1 p2 degree revs]."
  [rows]
  (reduce (fn [deps [p1 p2]]
            (let [existing1 (get deps p1 #{})
                  extended1 (conj existing1 p2)
                  existing2 (get deps p2 #{})
                  extended2 (conj existing2 p1)]
              (-> deps
                  (assoc-in [p1] extended1)
                  (assoc-in [p2] extended2))))
          {}
          rows))

(defn packages-in
  [deps]
  (sort (keys deps)))

(defn- boolean->int
  [b]
  (if b 1 0))

(defn- dependencies-of
  [packages [_node node-deps]]
  (map (comp boolean->int some? (partial node-deps)) packages))

(defn- sort-by-name
  "We want to map over a seq that is sorted on
   the same order as the packages names to
   generate the dependency matrix."
  [deps]
  (->> deps (into []) (sort-by first)))

(defn- presentation-name
  [n]
  (->> (s/split n #"/")
       reverse
       (take 2)
       reverse
       (s/join "/")))

(defn dependency-graph-from
  [rows]
  (let [deps (matrix-from rows)
        packages (packages-in deps)
        sorted-deps (sort-by-name deps)
        dep-matrix (map (partial dependencies-of packages) sorted-deps)]
    {:packageNames (map presentation-name packages)
     :matrix dep-matrix}))

(defn generate-dependency-matrix
  [coupling-file out-file-name]
  (with-open [out-file (io/writer out-file-name)]
    (let [couples (shared/read-csv-sans-header-from coupling-file)
          result (dependency-graph-from couples)]
      (json/write result out-file)
      result)))

