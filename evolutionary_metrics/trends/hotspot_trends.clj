;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.trends.hotspot-trends
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [evolutionary-metrics.mergers.shared-mergers :as shared]))

;;; This module calculates hotspot trends between two different analyses.
;;; We calculate the relative change within each hotspot. The idea is to 
;;; identify hotspots that climb the ranks rapidly so that we can flag them 
;;; as an early warning.
;;; On a similiar note, we'd also be able to identify hotspots that seem to 
;;; stabilize (i.e. less activity).

(defn- as-int
  [n]
  (Integer/parseInt n))

(defn- read-hotspots-from
  [f]
  (->> f
       shared/read-csv-sans-header-from
       (map-indexed (fn [rank [name revs]]
                      {:name name :revisions (as-int revs) :rank rank}))))

(defn- read-hotspots-filtered-on-code-size-from
  "We want to make it possible to ignore small files since these aren't true hotspots...yet."
  [f {:keys [min-code-size]}]
  (->> (shared/read-csv-sans-header-from f)
       (map-indexed (fn [rank [name revs code-size]]
                      {:name name :revisions (as-int revs) :rank rank :code-size (as-int code-size)}))
       (filter (fn [{:keys [code-size]}] (>= code-size min-code-size)))))

(defn- as-revision-increase
  [v1 v2]
  (- v2 v1))

(defn- as-rank-climb
  [r1 r2]
  (max 0
       (- r1 r2)))

(def ^:private revs-rank-extractor (juxt :revisions :rank))

(defn- as-delta-statistics
  [old-values new-values]
  (let [[revs1 rank1] (revs-rank-extractor old-values)
        [revs2 rank2] (revs-rank-extractor new-values)]
    [(as-revision-increase revs1 revs2)
     rank1
     rank2
     (as-rank-climb rank1 rank2)]))

(defn- default-for-new-spots
  [{:keys [revisions rank]} total-hotspots]
  [revisions total-hotspots rank (as-rank-climb total-hotspots rank)])

(defn passes-thresholds?
  [old-values new-values {:keys [min-revisions min-rank-increase]}]
  (let [[old-revs old-rank] (revs-rank-extractor old-values)
        [new-revs new-rank] (revs-rank-extractor new-values)]
    (and (>= (- new-revs old-revs) min-revisions)
         (< new-rank old-rank)
         (<= min-rank-increase (- old-rank new-rank)))))

(defn- spots-within-rank-cutoff
  [all-spots {:keys [min-new-rank]}]
  (filter (fn [{:keys [rank]}] (<= rank min-new-rank)) all-spots))

(defn as-ranked-hotspots
  [oldest newest {:keys [min-rank-increase] :as thresholds}]
  (let [old-spot-lookup (->> oldest (map (fn [{:keys [name] :as spot}] [name spot])) (into {}))
        new-to-consider (spots-within-rank-cutoff newest thresholds)
        total-hotspots (count oldest)]
    (for [{:keys [name] :as new-values} new-to-consider
          :let [old-values (get old-spot-lookup name)
                delta (if old-values
                        (as-delta-statistics old-values new-values)
                        (default-for-new-spots new-values total-hotspots))
                rank-increase (last delta)]
          :when (and (<= min-rank-increase rank-increase)
                     (or (nil? old-values) ; always include new Hotspots that are within the ranked window
                         (passes-thresholds? old-values new-values thresholds)))]
      (into [name] delta))))

(defn ranked-hotspot-delta
  ([oldest-file newest-file thresholds]
   (let [olds (read-hotspots-from oldest-file)
         news (read-hotspots-filtered-on-code-size-from newest-file thresholds)
         rankings (as-ranked-hotspots olds news thresholds)]
     (csv/write-csv *out* [["name" "revision" "oldrank" "newrank" "rank"]])
     (csv/write-csv *out* rankings)
     rankings))
  ([oldest-file newest-file thresholds out-file-name]
   (with-open [out-file (io/writer out-file-name)]
     (binding [*out* out-file]
       (ranked-hotspot-delta oldest-file newest-file thresholds)))))
