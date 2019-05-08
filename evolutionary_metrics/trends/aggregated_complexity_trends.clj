(ns evolutionary-metrics.trends.aggregated-complexity-trends
  (:require [evolutionary-metrics.mining.ws-complexity :as wsc]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.core :as core]
            [clj-time.core :as tc]
            [clj-time.format :as tf]
            [clojure.java.io :as io]
            [clojure.core.matrix :as matrix]
            [evolutionary-metrics.trends.dates :as dates]
            [clojure.data.csv :as csv]
            [evolutionary-metrics.mergers.shared-mergers :as shared])
  (:import (org.joda.time Interval)))

;; The overall algorithm:
;; 1. Sample each week for the past month.
;; 2. Sample every second month for the past year. Always start on the 1st of each month.
;; 3. Sample once per year for the history that's older than one year. Always start on YYYY-01-01.
;;
;; The reason we hardcode the start day/month is because we want to be able to compare trends
;; from different analyses run at later dates with earlier ones using the same samples points
;; for the shared interval.

(defn- samples-for
  [now step-unit-fn steps]
  (map (fn [i] (tc/minus now (step-unit-fn i))) (range steps)))

(defn- sample-points-for-recent-past
  [now]
  (let [the-past (tc/minus now (tc/months 1))
        y (tc/year the-past)
        m (tc/month the-past)
        end-recent (tc/date-time y m 1) ; we always want to start on the first of the past month
        weeks (tc/in-weeks (tc/interval end-recent now))]
    {:period-start end-recent
     :sample-points (samples-for now tc/weeks weeks)}))

(defn- sample-points-for-past-year
  [now]
  (let [one-year-earlier (tc/minus now (tc/years 1))]
    {:period-start one-year-earlier
     :sample-points (samples-for now (comp tc/months (partial * 2)) 6)}))

(defn- sample-points-for-old-history
  [now start-of-history]
  (let [y (tc/year now)
        sample-start (tc/date-time y 1 1)]
    (if (tc/before? sample-start start-of-history)
      {:period-start start-of-history
       :sample-points []}
      (let [ys (tc/in-years (tc/interval start-of-history sample-start))]
        {:period-start start-of-history
         :sample-points (samples-for sample-start tc/years (inc ys))}))))

(s/def ::analysis-interval #(instance? Interval %))
(s/def ::sample-points (s/coll-of ::core/datetime))

(s/fdef sample-points-in
        :args (s/cat :analysis-interval ::analysis-interval)
        :ret ::sample-points)

(defn sample-points-in
  "Calculates a number of sample points, each one a specific date, in the
   given analysis interval."
  [analysis-interval]
  (let [start (tc/start analysis-interval)
        end (tc/end analysis-interval)
        recent (sample-points-for-recent-past end)
        mid (sample-points-for-past-year (:period-start recent))
        old (sample-points-for-old-history (:period-start mid) start)]
    (->> [{:sample-points [start]} old mid recent]
         (mapcat :sample-points)
         distinct
         (remove (fn [d] (tc/before? d start)))
         sort)))

(s/def ::complexity-value nat-int?)
(s/def ::lines-of-code-value nat-int?)
(s/def ::lines-of-comments-value nat-int?)

(s/def ::complexity-point-in-time (s/tuple ::core/datetime ::complexity-value ::lines-of-code-value ::lines-of-comments-value))

(s/def ::complexity-trend (s/coll-of ::complexity-point-in-time))

(defn- update-aggregation-context
  [{:keys [acc]} date complexity-row trend-tail]
  (let [new-p (->> complexity-row (drop 1) vec)
        v (into [date] new-p)]
    {:current-value complexity-row
     :trend-tail trend-tail
     :acc (conj acc v)}))

(defn- date-of-point [p] (first p))

(s/fdef trend->complexity-at-sample-points
        :args (s/cat :trend ::complexity-trend
                     :sample-dates ::sample-points)
        :ret ::complexity-trend)

(defn fit-trend-to-sample-dates
  [sample-dates trend]
  (->> sample-dates
       (reduce (fn [{:keys [trend-tail current-value] :as context} date]
                 (let [[older newer] (split-with (fn [p] (not (tc/after? (date-of-point p) date))) trend-tail)
                       matching-row (last older)]
                   (if matching-row
                     (update-aggregation-context context date matching-row newer)
                     (update-aggregation-context context date current-value trend-tail))))
               {:current-value [:placeholder-for-date 0 0 0]
                :trend-tail trend
                :acc []})
       :acc))

(defn parse-complexity-trend
  [date-parser-fn file-name]
  (->> file-name
       shared/read-csv-sans-header-from
       (map (fn [[date total n _mean _median _sd _max comments]]
              [(date-parser-fn date) (Math/round (Double/parseDouble total)) (Integer/parseInt n) (Integer/parseInt comments)]))))

(defn- initial-trend-value-for
  [sample-dates]
  (->> (fit-trend-to-sample-dates sample-dates [])
       (map (partial drop 1))))

(defn calculate-aggregated-trend-from
  [complexities sample-dates]
  (->> complexities
       (reduce (fn [acc individual-complexity]
                 (let [fitted (fit-trend-to-sample-dates sample-dates individual-complexity)
                       fitted-values (map (partial drop 1) fitted)] ; the dates are fixed so ignore them
                   (matrix/add acc fitted-values)))
               (initial-trend-value-for sample-dates))
       (map (fn [d [total n comments]]
              [(dates/date->string d) total n comments (wsc/complexity-by-loc total (float n))])
            sample-dates)))

(defn- files->complexity-seq
  [files]
  (let [parse-date (partial tf/parse (tf/formatters :year-month-day))]
    (->> files
         (map (fn [f] (.getPath f)))
         (map (partial parse-complexity-trend parse-date)))))

(defn aggregate-trends-for
  [files sample-dates out-file-name]
  (let [complexities (files->complexity-seq files)
        aggregated (calculate-aggregated-trend-from complexities sample-dates)]
    (with-open [out-file (io/writer out-file-name)]
      (csv/write-csv out-file [["date" "total" "n" "comments" "ratio"]])
      (csv/write-csv out-file aggregated))))


