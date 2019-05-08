(ns hotspots-x-ray.x-rayer
  (:require [hotspots-x-ray.miner :as miner]
            [hotspots-x-ray.languages.parser :as parser]
            [hotspots-x-ray.languages.specs :as languages-spec]
            [hotspots-x-ray.cleansing.functions :as cleanse]
            [hotspots-x-ray.recommendations.similarity :as similarity-detector]
            [hotspots-x-ray.recommendations.proximity :as proximity]
            [hotspots-x-ray.complexity-trends :as complexity-trends]
            [hotspots-x-ray.recommendations.code-complexity :as complexity]
            [hotspots-x-ray.content-resolver :as resolver]
            [hotspots-x-ray.thresholds :as xray-thresholds]
            [evolutionary-metrics.analysis.entities :as maat-entities]
            [evolutionary-metrics.analysis.logical-coupling :as maat-temporal]
            [hotspots-x-ray.recommendations.code-statistics :as code-statistics]
            [clojure.set :as set]
            [taoensso.timbre :as timbre]
            [clojure.spec.alpha :as s])
  (:import (java.io FileReader)))

;; Algoritm:
;;
;; 1. Fetch all historic revisions.
;; 2. Sort them on oldest first. Use a moving window two pick two revisions at a time.
;; 3. Calculate functions in the oldest.
;; 4. Diff the two revisions, look at which functions that got changed.
;; 5. Move on to the next partition, back two #3.
;; Finally, filter-out everything that isn't included in the final (last) revision.

(def ^:private author-info "none")

(defn- adapt-to-maat-format
  "Pretend our metrics are mined on the same format as
   our 'normal' evolutionary metrics because we want
   to use the algorithms from closed-maat for the analyses."
  [changes]
  (->> changes
       (map (fn [{:keys [changes rev date]}]
              (map (fn [{:keys [name]}] {:author author-info :rev rev :date date :entity name}) changes)))
       (apply concat)))

(defn- current-stat-for
  [file-name current-file]
  (->> current-file
       FileReader.
       ;; TODO: autodetect encoding instead of using pure FileReader ?
       ;; see
       ;; Note that this affects more places, e.g. "View Code" functionality
       ;; and probably also complexity and biomarkers too
       #_(file-utils/reader-with-autodetected-encoding)
       (parser/parse-function-statistics file-name)))

(defn- combine-overloads
  "Some languages allow overloaded functions/methods. We combine them into a single
   unit of analysis as that's the concept that makes sense to reason about; Most overloads
   probably have to be modified/considered together as they're part of the same logical design element.
   However, to avoid user confusion (yes, that's been the case) we also include a counter that
   indicates that we're dealing with overloads here."
  [m [n loc cc]]
  (update-in m [n] (fn [[old-loc old-cc n-overloads]]
                     (let [new-loc (if (nil? old-loc) loc (+ old-loc loc))
                           new-cc (if (or (nil? old-cc) (= "-" old-cc))
                                    cc
                                    (str (+ (Integer/parseInt cc) (Integer/parseInt old-cc))))
                           overload-cnt (if (nil? n-overloads) 1 (inc n-overloads))]
                       [new-loc new-cc overload-cnt]))))

(defn functions-with-complexity-stats
  [file-name current-file-content s]
  (let [lines-in-file (clojure.string/split-lines current-file-content)]
    (->> (complexity/by-functions-in-file file-name lines-in-file s)
         (map (fn [{:keys [name cc] :as function}]
                [name
                 (code-statistics/loc-from function)
                 cc]))
         (reduce combine-overloads {}))))

(defn- hotspots-in
  [file-name m current-version current-file unmodified-functions]
  (let [complexity-lookup (functions-with-complexity-stats file-name (slurp current-file) current-version)
        revs (maat-entities/as-dataset-by-revision m)
        unmodified (map (fn [n]
                          (let [cloc (get complexity-lookup n)]
                                {:module n :revisions 0 :code (first cloc) :cc (second cloc) :overloads (nth cloc 2)})) unmodified-functions)
        res (as-> revs $
                  (map (fn [[name commits]]
                         (let [cloc (get complexity-lookup name)]
                           {:module name :revisions commits :code (first cloc) :cc (second cloc) :overloads (nth cloc 2)})) $)
                  (concat $ unmodified)
                  (sort-by (juxt :revisions :code) $)
                  (reverse $)
                  (map (juxt :module :revisions :code :cc :overloads) $))]
    {:headers [:module :revisions :code :cc :overloads]
     :results res}))

(defn- comparable-function-from
  [f function-bodies]
  {:name f
   :body (get function-bodies f "")})

(defn- similarity-of
  [function-bodies f1 f2 thresholds]
  (let [b1 (comparable-function-from f1 function-bodies)
        b2 (comparable-function-from f2 function-bodies)
        {:keys [similarity]} (similarity-detector/similarity-in-percent b1 b2 thresholds)]
    similarity))

(defn- temporal-coupling-with-similarity
  [m current-version thresholds]
  {:pre [(s/valid? ::languages-spec/parsed-function-statistics current-version)]}
  (let [function-bodies (cleanse/current-function-bodies-in current-version)
        couples (maat-temporal/by-degree m thresholds)
        res  (->> couples
                  :analysis-result-values
                  (map (fn [[f1 f2 degree average-revs]]
                         [f1 f2 degree average-revs (similarity-of function-bodies f1 f2 thresholds)])))]
    {:headers [:entity :coupled :degree :average-revs :similarity]
     :results res}))

(defn- tabular-proximity
  [current-version temporal-coupling-results]
  (proximity/recommend-refactorings current-version temporal-coupling-results))

(defn- complexity-trend-candidates
  [hotspots {:keys [min-revisions-for-trend max-fns-for-trend] :or {min-revisions-for-trend 10 max-fns-for-trend 10}}]
  (->> hotspots
       :results
       (take max-fns-for-trend)
       (filter (fn [[_name revisions]] (>= revisions min-revisions-for-trend)))
       (map first)))

(defn- generate-complexity-trends
  [evolutionary-metrics candidate-fns output-file-fn]
  (when output-file-fn
    (timbre/trace "Generating complexity trends for " (pr-str candidate-fns))
    (doseq [c candidate-fns]
      (complexity-trends/write-trend-for evolutionary-metrics c (output-file-fn c)))))

(defn- analyze
  [file-name {:keys [raw-evolution evolution current-version current-file]} unmodified-functions {:keys [complexity-trend-file-name-fn] :as options}]
  (timbre/debug "X-Ray analysis with options: " (pr-str options))
  (let [raw-hotspots (hotspots-in file-name evolution current-version current-file unmodified-functions)
        trend-candidates (complexity-trend-candidates raw-hotspots options)
        raw-temporal-coupling (temporal-coupling-with-similarity evolution current-version options)
        keyed-temporal (map (partial zipmap (:headers raw-temporal-coupling)) (:results raw-temporal-coupling))
        proximity-results (tabular-proximity current-version keyed-temporal)]
    (generate-complexity-trends raw-evolution trend-candidates complexity-trend-file-name-fn)
    [{:analysis "Hotspots" :result raw-hotspots}
     {:analysis "Temporal Coupling" :result raw-temporal-coupling}
     {:analysis "Proximity" :result
                {:headers [:entity1 :entity2 :totalproximity]
                 :results (map (juxt :entity1 :entity2 :totalproximity) proximity-results)}}]))

(defn- function-names-from
  [stats]
  (->> stats (map :name) (into #{})))

(defn- remove-obsolete-functions
  "Removes all historic data for functions that are no
   longer included in the current state of a file."
  [current-function-names evolutions]
  (filter (comp current-function-names :entity) evolutions))

(defn- modified-function-names
  [ds]
  (->> ds
       (map :entity)
       (into #{})))

(defn- detect-unmodified-functions
  "Discover the functions/methods that haven't been touched because
   we want to include them as well for completeness.
   This is particular important when we analyse a shorter timespan
   in a project."
  [modified-names current-names]
  (set/difference current-names modified-names))

(defn as-analyzable-evolution
  [repo file-name evo-metrics]
  (let [current-file (resolver/as-child-path repo file-name)
        current-version (current-stat-for file-name current-file)
        current-function-names (function-names-from current-version)
        ds (adapt-to-maat-format evo-metrics)
        evolution (remove-obsolete-functions current-function-names ds)]
    {:raw-evolution evo-metrics
     :evolution evolution
     :current-version current-version
     :current-file current-file
     :current-function-names current-function-names}))

(defn describe-evolution-of
  [repo start-date file-name git-cmd thresholds]
  (let [m (miner/evolutionary-metrics-for repo start-date file-name git-cmd thresholds)]
    (as-analyzable-evolution repo file-name m)))

(defn x-ray
  [repo start-date file-name git-cmd thresholds]
  (let [{:keys [evolution current-function-names current-version] :as evolutionary-metrics} (describe-evolution-of repo start-date file-name git-cmd thresholds)
        modified-functions (modified-function-names evolution)
        unmodified-functions (detect-unmodified-functions modified-functions current-function-names)]
    (timbre/trace "Evolution: " (pr-str evolution))
    (timbre/trace "Current version: " (pr-str (map (fn [m] (dissoc m :body)) current-version)))
    (analyze file-name evolutionary-metrics unmodified-functions (xray-thresholds/ensure-defaults thresholds))))
