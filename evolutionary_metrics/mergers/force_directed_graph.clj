;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.mergers.force-directed-graph
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.data.json :as json]))

;;; This module generates a JSON document suitable for a 
;;; D3 force directed graph layout.
;;; We also include the complexity of each node, typically 
;;; because we want to scale the visualized node with respect 
;;; to size.

(defn- read-csv-from
  [file]
  (with-open [in-file (io/reader file)]
    (doall
     (csv/read-csv in-file))))

(defn- as-indexed-node-names
  [coupling-results]
  (->> coupling-results
      (map (fn [[n1 n2 _ _]] [n1 n2]))
      flatten
      distinct
      (map-indexed (fn [i n] [n i]))
      (into {})))

(defn- as-links
  [indexed-nodes coupling-results]
  (map (fn [[n1 n2 degree revs]]
         {"source" (indexed-nodes n1)
          "target" (indexed-nodes n2)
          "value" degree
          "revs" revs})
       coupling-results))

(defn- read-coupling-results-from
  [coupling-file]
  (->> coupling-file
      read-csv-from
      (drop 1))) ; header

(defn- row-as-module-with-complexity
  [[_lang name _blank _comment code]]
  [name code])

(defn- modules-with-complexity
  [complexity-file]
  (->>
   (read-csv-from complexity-file)
   (drop 1) ; header
   (map row-as-module-with-complexity)
   (into {})))

(defn- as-nodes
  [indexed-nodes complexity-by-node]
  (->> indexed-nodes
       (into [])
       (sort-by second)
       (map (fn [[n _index]] 
              {"name" n
               "group" 1 ; TODO: we could use it to highlight different types or packages
               "size" (complexity-by-node n 1)}))))

(defn- as-force-directed-graph
  [complexity-file coupling-file]
  (let [coupling (read-coupling-results-from coupling-file)
        indexed-node-names (as-indexed-node-names coupling)
        links (as-links indexed-node-names coupling)
        complexity (modules-with-complexity complexity-file)
        nodes (as-nodes indexed-node-names complexity)]
    {"nodes" nodes
     "links" links}))

(defn generate-graph
  [complexity-file coupling-file out-file-name]
  (with-open [out-file (io/writer out-file-name)]
    (let [result (as-force-directed-graph complexity-file coupling-file)]
      (json/write result out-file))))
