(ns codescene.velocity.development-output
  (:require [codescene.analysis.paths :as paths]
            [semantic-csv.core :as sc]
            [evolutionary-metrics.trends.dates :as dates]
            [codescene.stats.math :as m]
            [clj-time.core :as tc]
            [clojure.java.io :as io]
            [cheshire.core :as json]))

(defn- ->velocity
  [{:keys [date revisions authors] :as c}]
  (-> c
      (assoc :date (dates/string->date date))
      (assoc :velocity (/ revisions (max authors 1)))))

(defn- weekly-velocity-from
  [revision-churn]
  (map ->velocity revision-churn))

(defn- percentage-increase-in
  [vnow vlast]
  (let [p (/ (- vnow vlast) vlast)]
    (-> p (* 100) float Math/round)))

(def ^:private no-trend-available {})

(defn- enough-samples-for-trend?
  [now last]
  (and (seq now)
       (seq last)))

(defn- trend-for
  [period-fn now vs]
  (let [last-period-start (tc/minus now (period-fn 2))
        last-period-end (tc/minus now (period-fn 1))
        last-period-samples (remove (fn [{:keys [date]}]
                                     (or (tc/before? date last-period-start)
                                         (tc/after? date last-period-end)))
                                   vs)
        this-period-samples (remove (fn [{:keys [date]}]
                                     (tc/before? date last-period-end))
                                   vs)]
    (if (enough-samples-for-trend? this-period-samples last-period-samples)
      (let [last-period-velocity-stats (m/stats-for (map :velocity last-period-samples))
            last-period-author-stats (m/stats-for (map :authors last-period-samples))
            velocity-stats (m/stats-for (map :velocity this-period-samples))
            author-stats (m/stats-for (map :authors this-period-samples))
            last-period-velocity (:mean last-period-velocity-stats)
            last-period-authors (:mean last-period-author-stats)
            period-velocity (:mean velocity-stats)
            period-authors (:mean author-stats)]
        {:monthly-velocity-delta (percentage-increase-in period-velocity last-period-velocity)
         :monthly-mean-authors period-authors
         :monthly-authors-delta (- period-authors last-period-authors)})
      no-trend-available)))

(defn- has-data-for-last-two-weeks?
  [now last]
  (when (and (some? now)
             (some? last))
    (let [diff (tc/interval (:date last) (:date now))]
      (< (tc/in-days diff) 8))))

(defn- weekly-trend-in
  [vs]
  (let [rvs (reverse vs)
        now (first rvs)
        last (second rvs)]
    (if (has-data-for-last-two-weeks? now last)
      {:weekly-velocity-delta (percentage-increase-in (:velocity now) (:velocity last))
       :weekly-authors-delta (- (:authors now) (:authors last))}
      no-trend-available)))

(defn- trends-in
  [{:keys [age-time-now]} vs]
  (let [now (dates/string->date age-time-now)]
    (merge
      (trend-for tc/months now vs)
      (weekly-trend-in vs))))

(defn velocity-delta
  [context revision-churn]
  (->> revision-churn
       weekly-velocity-from
       (trends-in context)))

(defn velocity-delta-from-file-results
  [{:keys [analysis-path-fn] :as context} dest-file-name]
  (let [churn (analysis-path-fn paths/revision-trend-csv)
        destination (analysis-path-fn dest-file-name)
        delta-statistics (->> (sc/slurp-csv churn)
                              (sc/cast-with {:revisions sc/->int :authors sc/->int})
                              (velocity-delta context))]
    (json/generate-stream delta-statistics (io/writer destination))))
