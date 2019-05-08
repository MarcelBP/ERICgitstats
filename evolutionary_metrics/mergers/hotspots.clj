;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.mergers.hotspots
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.trends.specs :as trends-specs]
            [clojure.spec.alpha :as s]))

;;; Mergers are responsible for combining different metrics 
;;; for the same modules into a presentable view.

(defn- modules-with-revs
  [revisions-file]
  (->>
   (shared/read-csv-from revisions-file)
   (drop 1) ; header
   (into {})))

(defn- merge-existing
  [revs-map complexity-modules]
  (sort-by
   (comp bigdec second)
   >
   (for [[name complexity] complexity-modules
         :let [matching-revs (revs-map name 0)]]
     [name matching-revs complexity name])))

(defn hotspots-by-complexity-and-revisions
  ([complexity-file revisions-file]
   (let [revs (modules-with-revs revisions-file)
         complexity (shared/modules-with-complexity complexity-file)
         merged (merge-existing revs complexity)]
     (csv/write-csv *out* [["module" "revisions" "code" "id"]])
     (csv/write-csv *out* merged)))
  ([complexity-file revisions-file out-file-name]
   (with-open [out-file (io/writer out-file-name)]
     (binding [*out* out-file]
       (hotspots-by-complexity-and-revisions complexity-file revisions-file)))))

;;
;; Hotspots by Relative Code Churn.
;;

(defn- name-and-metric
  [[name churn]]
  [name (Integer/parseInt churn)])

(defn- modules-with-added-churn
  [churn-file]
  (->> (shared/read-csv-sans-header-from churn-file)
       (map name-and-metric)
       (into {})))

(defn- modules-with-complexity
  [complexity-file]
  (map (fn [[_lang name blank comment code]] ; NOTE: our churn metric cannot separate blands and comments from code so include them all
         [name (+ (Integer/parseInt blank) (Integer/parseInt comment) (Integer/parseInt code))])
       (shared/read-csv-sans-header-from complexity-file)))

(defn- relative-churn
  [abs-churn current-loc]
  (->> (max current-loc 1)
       (/ abs-churn)
       (* 100)
       int))

(defn- as-complexity-with-relative-churn
  [added-churn-by-entity complexity]
  (for [[name complexity] complexity
        :let [matching-churn (get added-churn-by-entity name 0)]]
    [name (relative-churn matching-churn complexity) complexity]))

(defn as-spots-by-relative-churn
  [added-churn-by-entity complexity]
  (sort-by second >
   (as-complexity-with-relative-churn added-churn-by-entity complexity)))

(defn hotspots-by-relative-code-churn
  "Ranks hotspots by their relative code churn.
   Our churn input is the absolute churn of each entity.
   That raw metric is converted to a relative churn value and
   combined with the current complexity of the corresponding file
   to form hotspots."
  ([complexity-file churn-by-entity-file]
   (let [absolute-churn (modules-with-added-churn churn-by-entity-file)
         complexity (modules-with-complexity complexity-file)
         hotspots (as-spots-by-relative-churn absolute-churn complexity)]
     (csv/write-csv *out* [["module" "churn" "code"]])
     (csv/write-csv *out* hotspots)))
  ([complexity-file churn-by-entity-file out-file-name]
   (with-open [out-file (io/writer out-file-name)]
     (binding [*out* out-file]
       (hotspots-by-relative-code-churn complexity-file churn-by-entity-file)))))

;;
;; Once we have hotspots we can augment them with warnings on 
;; their complexity trends.
;;

(defn- as-indexed-warnings
  [trend-warnings-file]
  (->> (shared/read-csv-sans-header-from trend-warnings-file)
       (map (fn [[index yellow red]]
              [(Integer/parseInt index) [(Integer/parseInt yellow) (Integer/parseInt red)]]))
       (into {})))

(defn- as-warning-label
  [[yellow red]]
  (if (= 1 red)
    "Red"
    (if (= 1 yellow)
      "Yellow"
      "None")))

(defn- augment-hotspots
  [hotspots warning-lookup]
  (map (fn [spot]
         (if-let [warning (get warning-lookup (first spot))]
           (conj spot (as-warning-label warning))
           (conj spot "None")))
       hotspots))

(s/fdef augment-hotspots-with-complexity-trend-warnings
        :args (s/cat :hotspots-file ::core/filename
                     :hotspots-with-warnings ::trends-specs/hotspots-with-complexity-trend-warnings
                     :out-file-name ::core/filename)
        :ret nil?)

(defn augment-hotspots-with-complexity-trend-warnings
  [hotspots-file hotspots-with-warnings out-file-name]
  (let [hotspots (shared/read-csv-sans-header-from hotspots-file)
        warning-lookup (->> hotspots-with-warnings (map (juxt :hotspot-name :complexity-trend-classification)) (into {}))
        augmented (augment-hotspots hotspots warning-lookup)]
    (with-open [out-file (io/writer out-file-name)]
      (csv/write-csv out-file [["module" "revisions" "code" "id" "warning"]])
      (csv/write-csv out-file augmented))))
