(ns hotspots-x-ray.recommendations.coupling
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [evolutionary-metrics.analysis.math :as math]
            [clojure.math.numeric-tower :as numeric]
            [hotspots-x-ray.recommendations.similarity :as similarity-detector]))

(defn- shared-revisions
  [s]
  (->> s
       (map :changes-by-revision)
       (map (partial map :rev))
       (map (partial into #{}))
       (apply set/intersection)))

(defn coupled-revisions-in
  [evo-metrics-by-file]
  (->> (combo/combinations evo-metrics-by-file 2)
       (map shared-revisions)
       (apply set/union)))

(defn- order-changes-by-revision
  "Operates on sequences of maps like {:changes [f3 g3] :rev 1} and
  converts them to a map of rev => changes."
  [revs-of-interest {:keys [file-name changes-by-revision]}]
  (->> changes-by-revision
       (filter (comp revs-of-interest :rev))
       (map (fn [{:keys [changes rev]}] [rev changes]))
       (into {})
       (vector file-name)))

(defn- as-functions-in-file
  [[file-name changes]]
  (map (fn [f] {:file-name file-name :function f}) changes))

;; Calculate a seq like this:
;; [{:file-name "file1" :function "f1"}
;;  {:file-name "file2" :function "f2"}
;;  {:file-name "file3" :function "f3"}]
(defn- couples-in-revision
  [changes-by-revision rev]
  (as-> changes-by-revision $
        (map (fn [[name by-rev]] [name (get by-rev rev)]) $)
        (map as-functions-in-file $)
        (apply concat $)
        (combo/combinations $ 2)))

(defn- function-presentation-name
  [{:keys [file-name function]}]
  (str file-name "/" function))

(defn- ceil-as-int
  [n]
  (int (numeric/ceil n)))

(defn- couple-with-coupling
  [file-revisions-by-name [[p1 p2] shared-revs]]
  (let [r1 (get file-revisions-by-name (:file-name p1))
        r2 (get file-revisions-by-name (:file-name p2))
        average-revs (math/average r1 r2)
        coupling (math/as-percentage (/ shared-revs average-revs))]
    [p1 p2 (ceil-as-int coupling) (ceil-as-int average-revs)]))

(defn- couple-to-presentation
  [[p1 p2 degree average-revs similarity]]
  [(function-presentation-name p1) (function-presentation-name p2) degree average-revs similarity])

(defn- filter-on-thresholds
  "Since we always want to deliver something sensible we fetch a given result set and extend it with
   all other values that meet the thresholds, if any."
  [{:keys [min-coupling-degree min-revisions min-result-set-size] :or {min-result-set-size 10}} couples]
  (let [[minimum-result-set threshold-based-result-set] (split-at min-result-set-size couples)]
    (concat minimum-result-set
            (->> threshold-based-result-set
                 (filter (fn [[_p1 _p2 _degree revs]] (<= min-revisions revs)))
                 (filter (fn [[_p1 _p2 degree]] (<= min-coupling-degree degree)))))))

(defn- comparable-body-of
  [bodies-by-file {:keys [file-name function] :as n}]
  (let [fns (get bodies-by-file file-name)]
    {:name (function-presentation-name n)
     :body (get fns function "")}))

(defn- similarity-of
  "Calulates possible code duplication by a look-up of the
   current version of the couples functions."
  [bodies-by-file c1 c2 thresholds]
  (let [b1 (comparable-body-of bodies-by-file c1)
        b2 (comparable-body-of bodies-by-file c2)
        {:keys [ignored similarity]} (similarity-detector/similarity-in-percent b1 b2 thresholds)]
    (if (some? ignored)
      0
      similarity)))

(defn- extend-with-similarity
  [bodies-by-file thresholds couple]
  (let [c1 (first couple)
        c2 (second couple)
        similarity (similarity-of bodies-by-file c1 c2 thresholds)]
    (into couple [similarity])))

(defn- to-presentation
  [bodies-by-file file-revisions-by-name thresholds couple-freq]
  (->> couple-freq
       (sort-by second >)
       (map (partial couple-with-coupling file-revisions-by-name))
       (filter-on-thresholds thresholds)
       (map (partial extend-with-similarity bodies-by-file thresholds))
       (map couple-to-presentation)))

(defn- sum-temporal-couples
  [couples-by-rev]
  (->> couples-by-rev
       (apply concat)
       (remove (fn [[f1 f2]] (= (:file-name f1) (:file-name f2))))
       frequencies))

(defn- order-by-revisions-of-file
  [evo-metrics-by-file]
  (->> evo-metrics-by-file
       (map (fn [{:keys [file-name revisions]}] [file-name revisions]))
       (into {})))

(defn- function-bodies-by-file
  [evo-metrics-by-file]
  (->> evo-metrics-by-file
       (map (fn [{:keys [file-name current-function-bodies]}] [file-name current-function-bodies]))
       (into {})))

(defn coupled-functions-in
  "Receives a sequence of maps where each map represents the changes made
   to the functions in that file. The maps have the following keys:
    file-name: the name of the file being X-Rayed
    changes-by-revision: seqs of changed function per revision."
  [evo-metrics-by-file thresholds]
  (let [revs-of-interest (coupled-revisions-in evo-metrics-by-file)
        changes-by-revision (map (partial order-changes-by-revision revs-of-interest) evo-metrics-by-file)
        file-revisions-by-name (order-by-revisions-of-file evo-metrics-by-file)]
    (->> revs-of-interest
         (map (partial couples-in-revision changes-by-revision))
         sum-temporal-couples
         (to-presentation (function-bodies-by-file evo-metrics-by-file) file-revisions-by-name thresholds))))

(defn coupled-functions-to-csv
  [evo-metrics-by-file thresholds out-file-name]
  (with-open [out-file (io/writer out-file-name)]
    (let [couples (coupled-functions-in evo-metrics-by-file thresholds)]
      (csv/write-csv out-file [["entity" "coupled" "degree" "average-revs" "similarity"]])
      (csv/write-csv out-file couples))))
