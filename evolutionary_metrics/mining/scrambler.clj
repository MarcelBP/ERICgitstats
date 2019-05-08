(ns evolutionary-metrics.mining.scrambler
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [evolutionary-metrics.mergers.shared-mergers :as shared]))

(defn scramble-authors
  [content]
  (->> (map first content)
       distinct
       (sort-by count)
       (map-indexed (fn [i n] [n (str "Trial Author " (inc i))]))
       (into {})))

(defn- scramble-content
  [scrambled-names scrambled-authors [author rev date entity message loc-added loc-deleted]]
  [(get scrambled-authors author author) rev date (get scrambled-names entity entity) message loc-added loc-deleted])

(defn scramble-evo-data
  ([scrambled-names content]
   (let [authors (scramble-authors content)
         scrambled-content (map (partial scramble-content scrambled-names authors) content)]
       (csv/write-csv *out* [["author" "rev" "date" "entity" "message" "loc-added" "loc-deleted"]])
       (csv/write-csv *out* scrambled-content)))
  ([scrambled-names input-file-name out-file-name]
   (let [content (shared/read-csv-sans-header-from input-file-name)]
     (with-open [out-file (io/writer out-file-name)]
       (binding [*out* out-file]
         (scramble-evo-data scrambled-names content)))
     out-file-name)))
