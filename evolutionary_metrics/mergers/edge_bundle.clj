;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.mergers.edge-bundle
  (:require [clojure.java.io :as io]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [clojure.data.json :as json]
            [clojure.string :as str]))

;;; This module generates a JSON document suitable for a 
;;; D3 hierarchical edge bundle layout.

(defn- as-hierarchical-name
  [n]
  (str/replace n #"[/\\]" "."))

(defn- coupling->node-names
  [[n1 n2 _ _]]
  [n1 n2])

(def as-node-names (partial shared/as-node-names coupling->node-names))

(defn couples-for
  [n coupling-results]
  (->> coupling-results
       (filter (fn [[n1 n2]]
                 (or (= n n1) (= n n2))))
       (map (fn [[n1 n2 degree]]
              (cond
                (= n n1) [n2 degree]
                (= n n2) [n1 degree])))
       (map (fn [[name degree]] {:name (as-hierarchical-name name) :degree degree}))))

(defn- hierarchical-by-node
  "This function has a terrible run-time complexity.
   Is there a better way?"
  [couples]
  (for [node-name (as-node-names couples)
        :let [hierarchical-couples (couples-for node-name couples)
              hierarchical-node (as-hierarchical-name node-name)]]
    {:imports hierarchical-couples
     :name hierarchical-node
     :displayname node-name
     :size 1}))

(defn as-dependencies-by-node
  [couples]
  {:version 2
   :couples (hierarchical-by-node couples)})

(defn generate-bundle
  [coupling-file out-file-name]
  (with-open [out-file (io/writer out-file-name)]
    (let [couples (shared/read-csv-sans-header-from coupling-file)
          result (as-dependencies-by-node couples)]
      (json/write result out-file))))
