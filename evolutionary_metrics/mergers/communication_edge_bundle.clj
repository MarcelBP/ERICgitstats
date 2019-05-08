;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.mergers.communication-edge-bundle
  (:require [clojure.java.io :as io]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [clojure.data.json :as json]
            [clojure.string :as str]))

(defn- communication->node-names
  [[n1 n2 _ _ _]]
  [n1 n2])

(def as-node-names (partial shared/as-node-names communication->node-names))

(def couples-for (partial shared/couples-for communication->node-names))

(defn- as-dependencies-by-node
  "This function has a terrible run-time complexity.
   Is there a better way?"
  [nodes coupling-results]
  (for [node nodes
        :let [coupled (couples-for node coupling-results)]]
    {:imports coupled 
     :name node 
     :size 1}))

(defn generate-bundle
  [communication-file out-file-name]
  (with-open [out-file (io/writer out-file-name)]
    (let [couples (shared/read-csv-sans-header-from communication-file)
          nodes (as-node-names couples)
          result (as-dependencies-by-node nodes couples)]
      (json/write result out-file))))
