;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.trends.complexity-trend-step-detection
  (:require [clj-time.core :as tc]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.parsers.time-parser :as tp]
            [evolutionary-metrics.trends.specs :as trends-specs]
            [semantic-csv.core :as sc]
            [incanter.stats :as stats]))

;;; This module is responsible for detecting significant steps in a complexity trend.
;;;
;;; We use this algorithm to generate early warnings. A future use may be to look for 
;;; downward steps too, as they would signal refactorings.
;;;
;;; The general algorithm is:
;;; 1. Read a complexity trend as a dataset.
;;;
;;; 2. Filter the dataset on time, for example only consider the past 6 months.
;;;    The rationale is to filter away old spikes that have been addressed.
;;;
;;; 3. Calulcate a distance between the Lines of Code and the actual Complexity for
;;;    each sample point in the trend. We care about the distance, since an increase 
;;;    in distance cannot be explained by mere addition of code (not saying that the 
;;;    latter isn't a problem...).
;;;
;;; 4. Calculate a threshold for the step. We want something that signals a significant
;;;    increase so we use Standard Deviations from the Mean as our threshold.
;;;    We mark 1 SD steps as "yellow" warnings and 2 SD steps as "red" warnings.
;;;    In addition, we require a minimum threshold of 10% for "yellow" and 25% for red; We 
;;;    pick the maximum of either the SD threshold or the percentage (the rationale is that 
;;;    a long, flat curve will have a very low SD).
;;;
;;;    Note that the client may parameterize with other thresholds.
;;; 5. Check if the _last_ sample point exceeds the threshold. The reason we use the
;;;    last sample point is to avoid false positives:
;;;     - The dromedary pattern: A historic spike that has since been refactored.
;;;     - Sudden final spike: If we'd use (as previously attempted) a rolling average, it 
;;;       will take more sample points to detect the spike. That's not good enough since we 
;;;       want to react immediately.


(def ^:private no-warning-value {:yellow false :red false})

(def time-parser (tp/time-parser-from "yyyy-MM-dd"))

(defn relevant-slice-of
  "We want to possibly ignore old spikes in the data.
   We do that by slicing the data set in time. The user
   specifies the age, in months, of the time we shall consider."
  [ds months-of-history]
  (let [last-date (last (map :date ds))]
    (if (seq last-date)
      (let [last-date-time (time-parser last-date)
            first-date-to-consider (tc/minus last-date-time (tc/months (inc months-of-history)))]
        (filter (comp (fn [d]
                        (not (tc/before? (time-parser d) first-date-to-consider)))
                      :date) ds))
      ds)))

(defn- loc-delta
  [n total]
  (max (- total n) 0))

(defn- as-complexity-distance-from-loc-ds
  [ds]
  (map (fn [{:keys [n total] :as v}]
             (assoc v :distance (loc-delta n total)))
       ds))

(defn- descriptive-statistics-of
  [distance-ds]
  (let [distances (map :distance distance-ds)]
    {:mean (stats/mean distances)
     :sd (stats/sd distances)}))

(defn- step-warning-threshold-for
  [distance-ds {:keys [threshold-in-sd-yellow threshold-in-sd-red min-percentage-yellow min-percentage-red]}]
  (let [{:keys [mean sd]} (descriptive-statistics-of distance-ds)
        suggested-yellow (+ mean (* threshold-in-sd-yellow sd))
        suggested-red (+ mean (* threshold-in-sd-red sd))
        min-yellow (+ mean (* mean min-percentage-yellow))
        min-red (+ mean (* mean min-percentage-red))]
    [(max min-yellow suggested-yellow)
     (max min-red suggested-red)]))

(defn- past-threshold?
  [distance-ds [threshold-yellow threshold-red] hotspot-revisions min-sample-points]
  (let [values-of-interest (map :distance distance-ds)
        last-period (first (reverse values-of-interest))]
    (if (>= hotspot-revisions min-sample-points) ; do detection
           {:yellow (< threshold-yellow last-period)
            :red (< threshold-red last-period)}
           no-warning-value)))

(defn- trend-data-past-configured-threshold?
  "The user configures a minimum LoC threshold where we
   ignore smaller files. The rationale is that it's more
   risk for false positives in small files."
  [ds {:keys [min-loc]} cut-off-date]
  (let [final-v (last ds)
        final-loc (:n final-v)
        final-date (:date final-v)]
    (when (and (some? final-loc) (seq final-date))
      (and (>= final-loc min-loc)
           (tc/after? (time-parser final-date) cut-off-date)))))

(defn complexity-step-detected?
  [ds {:keys [trend-period-in-months min-sample-points hotspot-revisions] :as options} cut-off-date]
  (let [relevant-ds (relevant-slice-of ds trend-period-in-months)
        n-samples (count relevant-ds)]
    (if (< 1 n-samples)
      (if (trend-data-past-configured-threshold? relevant-ds options cut-off-date)
        (let [distance-ds (as-complexity-distance-from-loc-ds relevant-ds)
              threshold (step-warning-threshold-for distance-ds options)]
          (past-threshold? distance-ds threshold hotspot-revisions min-sample-points))
        no-warning-value)
      no-warning-value)))

(defn- complexity-step-in-file?
  [complexity-trend-dataset-fn trend-options cut-off-date]
  (try
    (let [complexity-ds (complexity-trend-dataset-fn)]
      (complexity-step-detected? complexity-ds trend-options cut-off-date))
    (catch java.io.FileNotFoundException _
      no-warning-value)))

(defn- with-classified-warnings
  [trend-options cut-off-date {:keys [hotspot-name hotspot-revisions complexity-trend-dataset-fn]}]
  (let [extended-trend-options (merge trend-options {:hotspot-revisions hotspot-revisions})
        {:keys [yellow red]} (complexity-step-in-file? complexity-trend-dataset-fn extended-trend-options cut-off-date)]
    {:hotspot-name hotspot-name
     :complexity-trend-classification [(if yellow 1 0) (if red 1 0)]}))

(s/fdef detect-increasing-steps-in
        :args (s/cat :hotspots ::trends-specs/hotspot-trend-files
                     :trend-options ::trends-specs/complexity-thresholds
                     :time-now ::core/datetime)
        :ret ::trends-specs/hotspots-with-complexity-trend-warnings)

(defn detect-increasing-steps-in
  "Generates a classifications of each given Hotspot trend where:
    Yellow: '1' in case the trend exceeded the threshold for a 'yellow' warning
    Red: '1' in case the trend exceeded the threshold for a 'red' warning.

   Note that trends are only detected for Hotspots with recent development activity (as
   determined by time-now and the trend-options)."
  [hotspots {:keys [trend-warning-look-back-in-months] :as trend-options} time-now]
  (let [cut-off-date (tc/minus time-now (tc/months trend-warning-look-back-in-months) (tc/days 1))]
    (map (partial with-classified-warnings trend-options cut-off-date) hotspots)))

(defn row->complexity-dataset
  [r]
  (zipmap [:date :total :n :mean :median :sd :max :comments :ratio] r))

(defn complexity-dataset-from-file
  [file-name]
  (->> file-name
       sc/slurp-csv
       (sc/cast-with {:n sc/->int
                      :mean sc/->float
                      :total sc/->int})))
    
