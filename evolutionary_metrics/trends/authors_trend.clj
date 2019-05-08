(ns evolutionary-metrics.trends.authors-trend
  (:require [clojure.spec.alpha :as s]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.trends.dates :as dates]
            [clj-time.core :as tc]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv])
  (:import (org.joda.time Interval)))

(s/def ::analysis-interval #(instance? Interval %))
(s/def ::start-date  ::core/datetime)
(s/def ::end-date  ::core/datetime)

(s/def ::author (s/keys :req-un [::start-date ::end-date]))
(s/def ::authors (s/coll-of ::author))

(s/def ::contributor-date ::core/datetime)
(s/def ::n-authors nat-int?)

(s/def ::contribution-statistic (s/tuple ::contributor-date ::n-authors))
(s/def ::contribution-statistics (s/coll-of ::contribution-statistic))

(defn- trend-points
  [analysis-interval]
  (let [start (tc/start analysis-interval)
        y (tc/year start)
        m (tc/month start)
        start-sample (tc/date-time y m 1)
        months (tc/in-months (tc/interval start-sample (tc/end analysis-interval)))]
    (map (fn [i] (tc/plus start-sample (tc/months i))) (range (inc months)))))

(defn- contributors-after
  [cut-off-date authors]
  (remove (fn [{:keys [end-date]}]
            (tc/before? end-date cut-off-date)) ; no longer active
          authors))

(defn- active-on-date
  [cut-off-date authors]
  (remove (fn [{:keys [start-date]}]
            (tc/after? start-date cut-off-date)) ; hasn't started yet
          authors))

(defn- authors-per-month
  [{:keys [active-authors acc]} cut-off-start-date]
  (let [remaining-authors (contributors-after cut-off-start-date active-authors)
        adjusted-start-date (tc/minus (tc/plus cut-off-start-date (tc/months 1)) (tc/days 1)) ; get all authors added during a month
        active (active-on-date adjusted-start-date remaining-authors)
        stats (conj acc [cut-off-start-date (count active)])]
    {:active-authors remaining-authors
     :acc stats}))

(defn- interpolate-final-sample-point
  [vs]
  (let [r (reverse vs)
        this-month (first r)
        past-month (second r)]
    (if (and (seq this-month) (seq past-month))
      (->> r  (drop 1) (concat [[(first this-month) (second past-month)]]) reverse)
      vs)))

(s/fdef contributing-authors-per-month
        :args (s/cat :analysis-interval ::analysis-interval
                     :author-statistics ::authors)
        :ret ::contribution-statistics)

(defn contributing-authors-per-month
  [analysis-interval author-statistics]
  (let [authors-by-date (sort-by :start-date author-statistics)]
    (->> analysis-interval
         trend-points
         (reduce authors-per-month
                 {:active-authors authors-by-date
                  :acc []})
         :acc
         interpolate-final-sample-point)))

(defn persist-contributing-authors-per-month
  [analysis-interval author-statistics out-file-name]
  (let [statistics (contributing-authors-per-month analysis-interval author-statistics)
        formatted (map (fn [[date authors]] [(dates/date->string date) authors]) statistics)]
    (with-open [out-file (io/writer out-file-name)]
      (csv/write-csv out-file [["date" "authors"]])
      (csv/write-csv out-file formatted))))
