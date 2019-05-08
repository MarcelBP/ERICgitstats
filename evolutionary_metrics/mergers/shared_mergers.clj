(ns evolutionary-metrics.mergers.shared-mergers
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(defn read-csv-from
  [file]
  (with-open [in-file (io/reader file)]
    (doall
      (csv/read-csv in-file))))

(defn read-csv-sans-header-from
  [file]
  (->> file
       read-csv-from
       (drop 1)))

(defn as-node-names
  [node-fn coupling-results]
  (->> coupling-results
      (map node-fn)
      flatten
      distinct))

(defn couples-for
  [node-fn n coupling-results]
  (->> coupling-results
       (map node-fn)
       (filter (fn [[n1 n2]]
                 (or (= n n1) (= n n2))))
       (map (fn [[n1 n2]]
                 (cond 
                  (= n n1) n2
                  (= n n2) n1)))))

(defn- row-as-module-with-complexity
  [[_lang name _blank _comment code]]
  [name code])

(defn modules-with-complexity
  [complexity-file]
  (->>
   (read-csv-from complexity-file)
   (drop 1) ; header
   (map row-as-module-with-complexity)))
