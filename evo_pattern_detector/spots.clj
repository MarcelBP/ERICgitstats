(ns evo-pattern-detector.spots
  (:require [kmeans-clj.core :refer [k-means]]
            [incanter.stats :refer [euclidean-distance]]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.data.csv :as csv]
            [clojure.data.json :as json]))

;;; This module identifies the two top clusters of hotspots.
;;;
;;; The idea is that once you've done a hotspot analysis, you want 
;;; a qualitative answer: is it good? Bad? Or in between?
;;
;;; By clustering our hotspots, we're able to identify the top groups 
;;; and make recommendations to the user.
;;; The algorithm is k-means clustering, which fits well on a tail heavy 
;;; curve like we have for all hotspots.

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

(defn- as-int
  [n]
  (Integer/parseInt n))

(def ^:const max-hotspots 100)
(def ^:cost max-fractals (+ max-hotspots 200)) ; limit the data and use defaults
(def ^:const max-socs max-fractals)

(defn- shorten-data-space
  "Or it will take forever and all your CPU to cluster it...
   The important thing is that we get enough of the tail."
  [n-data v]
  (take (min n-data (count v)) v))

(def shorten-hotspots-data-space (partial shorten-data-space max-hotspots))

;;
;; Read SoC
;;

(defn- read-soc-from
  "Reads the non-normalized soc values."
  [file]
  (->> file
       read-csv-sans-header-from
       (shorten-data-space max-socs)
       (map (fn [[entity soc]]
              [entity (as-int soc)]))))

(defn as-soc-lookup
  [socs]
  (let [max-soc-val (or (second (first socs)) 1) ; The input data is sorted
        min-soc-val (or (second (last socs)) 1)
        min-normalized-soc-val (/ min-soc-val max-soc-val)
        normalized-soc (map (fn [[n v]]
                              [n [(/ v max-soc-val) v]])
                            socs)
        soc-look-up (into {} normalized-soc)]
    (fn [entity]
      (if-let [v (get soc-look-up entity)]
        {:normalized-soc (first v)
         :soc (second v)}
        {:normalized-soc min-normalized-soc-val
         :soc min-soc-val}))))

;;
;; Read Fractal Values
;;

(defn- read-fragmentation-from
  "Note that the fractal value is already normalized."
  [file entities-of-interest]
  (->> file
       read-csv-sans-header-from
       (filter (fn [[entity & _rest]] (get entities-of-interest entity)))
       (map (fn [[entity fragmentation _ n-authors]]
              [entity [(bigdec fragmentation) n-authors]]))))

(defn- lookup-fragmentation-val-for
  [fragmentation val-fn]
  (->> fragmentation
       (map (comp first second))
       (apply val-fn)))

(defn as-fragmentation-lookup
  [fragmentation]
  (let [min-frag-val (if (empty? fragmentation) 0 (lookup-fragmentation-val-for fragmentation min))
        frags-by-name (into {} fragmentation)]
    (fn [entity]
      (if-let [v (get frags-by-name entity)]
        {:normalized-fragmentation (first v)
         :n-authors (second v)}
        {:normalized-fragmentation min-frag-val
         :n-authors 1}))))

(defn- converted-spots
  [d]
  (map (fn [[name revs loc _id trendwarning]] [name (as-int revs) (as-int loc) trendwarning]) d))

(defn- data-from 
  [d]
  (map (fn [[_ revs loc]] [revs loc]) d))

(defn- spot->cluster-data
  [{:keys [normalized-revs normalized-loc normalized-fragmentation normalized-soc]}]
  [normalized-revs normalized-loc normalized-fragmentation normalized-soc])

(defn- data-from-full-vectors
  [d]
  (map spot->cluster-data d))

(defn sort-by-category
  "Sort by the distance from origo. Note that we 
   have to take all dimensions into account and
   update this function everytime we add a vector."
  [s]
  (let [sorter (partial euclidean-distance [0 0 0 0])]
    (->> s
         (into [])
         (sort-by #(apply max (map sorter %)) >))))

(defn- total-code-size
  [spots]
  (->> spots
      (map (fn [[_ _ s]] s))
      (reduce +)))

(defn- top-spots-min-rev
  [f]
   (->> f
        read-csv-sans-header-from
        data-from
        shorten-hotspots-data-space
        (map first)
        (apply min)))

(defn- kmean-spots
  [v]
  (if (empty? v)
    v
    (k-means v
             euclidean-distance ; similarity function
             identity           ; our data is already grouped
             10                 ; optimal cluster size
             300)))             ; number of iterations

(defn hotspot-clusters-in
  "Returns the two top clusters with hotspots."
  [top-spots]
  (->> top-spots
       kmean-spots
       sort-by-category
       (take 2)
       (map #(sort-by first > %))))

(defn- as-flat-classification
  [clusters]
  (->> clusters
       (map-indexed (fn [i c] (map (fn [_v] i) c)))
       flatten))

(defn- max-loc
  [d]
  (if (empty? d)
    0
    (->> d
         (map (fn [[_ _ loc]] loc))
         (apply max))))

(defn- max-rev
  [d]
  (if (empty? d)
    0
    (->> d
         (map (fn [[_ rev _]] rev))
         (apply max))))

(defn- normalize-to-range
  [max-v v]
  (float (/ v (max max-v 1))))

(defn- normalize-spots-with-additional-data-vectors
  [spots fractal-lookup-fn soc-lookup-fn]
  (let [max-loc-val (max (max-loc spots) 1)
        max-rev-val (max (max-rev spots) 1)]
    (map (fn [[name revs loc trendwarning]]
           (->> {:name name
                 :normalized-revs (/ revs max-rev-val)
                 :revs revs
                 :normalized-loc (/ loc max-loc-val)
                 :loc loc
                 :trendwarning trendwarning}
                (merge (fractal-lookup-fn name))
                (merge (soc-lookup-fn name))))
         spots)))

;; TODO: we make a _lot_ of passes through the data - this needs serious refactoring!
(defn- clustered->spots
  [clusters top-spots]
  ;; build lookup map to find full hotspot data for clustered spot data (see `spot->cluster-data`)
  ;; notice that two different hotspots might share the same values therefore we need
  ;; `group-by`, `distinct` and `mapcat`.
  (let [clustered-spot-data->full-spot (group-by spot->cluster-data top-spots)
        ;; to handle duplicates we need to use distinct, otherwise hotspots would be duplicated
        ;; if 2 or more hotspots share the same clustered-data (nrevs, nlocs, etc.).
        flat-distinct-clusters (distinct (apply concat clusters))]
    (mapcat (fn [cluster] (get clustered-spot-data->full-spot cluster))
            flat-distinct-clusters)))

(defn- as-classified-hotspots
  [top-spots fractal-lookup-fn soc-lookup-fn clustering-fn]
  (let [normalized-spots (normalize-spots-with-additional-data-vectors 
                          top-spots 
                          fractal-lookup-fn
                          soc-lookup-fn)
        clusters (clustering-fn (data-from-full-vectors normalized-spots))
        classified (as-flat-classification clusters)
        clustered-spots (clustered->spots clusters normalized-spots)]
    (map (fn [classification full-spot]
           (merge full-spot {:classification classification}))
         classified clustered-spots)))

(defn- cluster-size
  [c]
  (reduce + (map :loc c)))

; shorten-hotspots-data-space

(defn- names-of
  [cluster]
  (->> (map :name cluster)
       (into #{})))

(defn- n-distinct-commits-in
  [changes]
  (->> changes (map second) distinct count))

(defn- as-relative-percentage-of
  [v total]
  (* 100
     (normalize-to-range total v)))

(defn- relative-percentage-of-revisions-in
  [cluster change-by-revisions-file]
  (let [entity-names (names-of cluster)
        all-changes (read-csv-sans-header-from change-by-revisions-file)
        total-revs (n-distinct-commits-in all-changes)
        revs-in-cluster (n-distinct-commits-in (filter (fn [[_ _ _ entity]] (entity-names entity)) all-changes))]
    (as-relative-percentage-of revs-in-cluster total-revs)))

(def ^:const industry-average-loc 1000000)

(defn- relative-cluster-sizes
  [classified-spots total-loc change-by-revisions-file]
  (let [clusters (group-by :classification classified-spots)
        c1 (clusters 0 [])
        c2 (clusters 1 [])
        c1-sum (as-relative-percentage-of (cluster-size c1) total-loc)
        c2-sum (as-relative-percentage-of (cluster-size c2) total-loc)
        c1-revisions (relative-percentage-of-revisions-in c1 change-by-revisions-file)
        loc-ref (max industry-average-loc total-loc)]
    [{:title "Hotspots"
     :subtitle "%"
     :ranges [5,20,100]
     :measures [c1-sum c2-sum]
     :markers [10] ; replace with industry average!
     :efforts [c1-revisions]}
     {:title "Code Size"
      :subtitle "LoC"
      :ranges [(/ loc-ref 10) (/ loc-ref 2) (+ loc-ref (/ loc-ref 20))]
      :measures [total-loc]
      :markers [industry-average-loc]}]))

(def ^:const min-number-of-commits-to-consider 10)
(def ^:const min-loc-to-consider 5000)

(defn- as-output-spots
  [spots]
  (map (fn [{:keys [name classification loc revs n-authors soc trendwarning]}]
         [name classification loc revs n-authors soc trendwarning])
       spots))

(defn hotspot-files->input-seq
  [f]
  (-> f
      read-csv-sans-header-from
      converted-spots))

;[name (as-int revs) (as-int loc) trendwarning]
(s/def ::name string?)
(s/def ::revs nat-int?)
(s/def ::loc nat-int?)
(s/def ::trendwarning string?)

(s/def ::hotspot (s/tuple ::name ::revs ::loc ::trendwarning))
(s/def ::hotspots (s/coll-of ::hotspot))

(s/def ::filename string?)
(s/def ::clustering-fn fn?)

(s/def ::prioritized-hotspots-result (s/keys :req-un
                                             [::prioritized-hotspots
                                              ::prioritization-statistics]))

(defn prioritize-hotspots-using
  "This is the actual classification and it's extracted into a side-effect free function
   for testing purposes."
  [change-by-revisions-file
   hotspots
   fragmentation-file
   soc-file
   clustering-fn] ; parameterize with the actual ML algorithm since the real one contains randomness and isn't suited to automated testing
  (let [total-loc (total-code-size hotspots)
        top-spots (shorten-hotspots-data-space hotspots)
        top-spots-names (->> top-spots (map first) (into #{}))
        fragmentation (read-fragmentation-from fragmentation-file top-spots-names)
        fragmentation-lookup-fn (as-fragmentation-lookup fragmentation)
        soc (read-soc-from soc-file)
        soc-lookup-fn (as-soc-lookup soc)
        classified-spots (as-classified-hotspots top-spots
                                                 fragmentation-lookup-fn
                                                 soc-lookup-fn
                                                 clustering-fn)
        presentable-spots (as-output-spots classified-spots)
        relative-spot-cluster-sizes (relative-cluster-sizes classified-spots total-loc change-by-revisions-file)]
    {:prioritized-hotspots presentable-spots
     :prioritization-statistics relative-spot-cluster-sizes}))

(s/fdef prioritize-hotspots-using
        :args (s/cat :change-by-revisions-file ::filename
                     :hotspots ::hotspots
                     :fragmentation-file ::filename
                     :soc-file ::filename
                     :clustering-fn ::clustering-fn)
        :ret ::prioritized-hotspots-result)

(defn classify-hotspots
  "Calculates a classification of each top hotspot.
   The result is delivered as CSV with [name, classification] columns.
   Note that the data should full-fill the pre-requisites in 
   suitable-for-analysis."
  [change-by-revisions-file
   hotspots
   fragmentation-file
   soc-file
   class-out-file-name
   stats-out-file-name
   {:keys [clustering-fn] :or {clustering-fn hotspot-clusters-in} :as _options}]
    (let [{:keys [prioritized-hotspots prioritization-statistics]}
          (prioritize-hotspots-using change-by-revisions-file hotspots fragmentation-file soc-file clustering-fn)]
      (with-open [class-out-file (io/writer class-out-file-name)
                  stats-out-file (io/writer stats-out-file-name)]
        (csv/write-csv class-out-file [["name" "classification" "loc" "revisions" "nauthors" "soc" "trendwarning"]])
        (csv/write-csv class-out-file prioritized-hotspots)
        (json/write prioritization-statistics stats-out-file))))
