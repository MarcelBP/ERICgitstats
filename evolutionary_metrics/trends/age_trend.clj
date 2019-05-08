;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.trends.age-trend
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.trends.specs :as trend-specs]))

;;; Calculates a frequncy distribution of the age of all 
;;; modules in a given codebase.
;;; Uses the result of an 'age' analysis as input.

(defn- read-existing-file-names-from
  "Returns a set of all file names in the 
   given LoC file."
  [f]
  (->> (shared/read-csv-sans-header-from f)
       (map second)
       (into #{})))

(defn- as-int
  [n]
  (Integer/parseInt n))

(defn- as-percentage
  [freq total]
  (Math/round
    (float
      (* 100
         (/ freq total)))))

(defn as-age-frequency
  [ages]
  (let [total-modules (count ages)]
    (->> (map (comp as-int second) ages)
         frequencies
         (into [])
         (map (fn [[age freq]] [age (as-percentage freq total-modules)]))
         (sort-by first))))

(defn filter-obsolete-files
  "Our VCS data often contain files that are 
   no longer present. We need to remove those from 
   the calculation, otherwise we'll bias the results."
  [names-with-age existing-files]
  (filter (fn [[name age]] (existing-files name)) names-with-age))

(s/fdef code-age-frequency
        :args (s/alt :std-out (s/cat :complexity-file ::core/filename
                                     :age-file ::core/filename)
                     :out-file (s/cat :complexity-file ::core/filename
                                      :age-file ::core/filename
                                      :out-file-name ::core/filename))
        :ret (s/coll-of (s/tuple int? int?)))

(defn code-age-frequency
  ([complexity-file age-file]
   (let [ages (shared/read-csv-sans-header-from age-file)
         existing-files (read-existing-file-names-from complexity-file)
         existing-ages (filter-obsolete-files ages existing-files)
         freqs (as-age-frequency existing-ages)]
     (csv/write-csv *out* [["age" "frequency"]])
     (csv/write-csv *out* freqs)
     freqs))
  ([complexity-file age-file out-file-name]
   (with-open [out-file (io/writer out-file-name)]
     (binding [*out* out-file]
       (code-age-frequency complexity-file age-file)))))
