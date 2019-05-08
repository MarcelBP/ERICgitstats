(ns evolutionary-metrics.analysis.author-timeline
  (:require [evolutionary-metrics.analysis.internal.time :as time]
            [evolutionary-metrics.trends.dates :as dates]
            [clj-time.core :as tc]
            [evolutionary-metrics.analysis.authors :as authors]))

(def ^:private ideal-days-in-trend 365)

(defn- a-year-ago-from
  "We need to align the date with the 1st of the next month due
   to a bug in the visualization lib that we use. This sucks, but
   we lose at maximum 4 weeks of the earliest churn."
  [now]
  (let [exact (tc/minus now (tc/days ideal-days-in-trend))
        next-month (tc/plus exact (tc/months 1))
        y (tc/year next-month)
        m (tc/month next-month)]
    (tc/date-time y m 1)))

(defn sample-dates-for
  [now]
  (let [start-date (a-year-ago-from now)
        days-in-trend (tc/in-days (tc/interval start-date now))]
    (->> days-in-trend
         range
         (map (fn [day] (tc/plus start-date (tc/days day))))
         (map dates/date->string))))

(defn- commits-for-past-year
  [commits a-year-ago]
  (remove (fn [{:keys [date]}]
            (tc/before? (dates/string->date date) a-year-ago))
          commits))

(defn- author-contribs
  [cs]
  (group-by :author cs))

(defn- unique-commits-for
  [cs]
  (->> cs (map :rev) distinct count))

(defn- daily-activity-on
  [active-authors sample-dates cs-by-date]
  (let [inactive-day (repeat (count active-authors) 0)]
    (map (fn [day]
           (if-let [cs (get cs-by-date day)]
             (let [by-author (author-contribs cs)
                   contribs (mapv (fn [a] (unique-commits-for (get by-author a []))) active-authors)]
               (concat [day] contribs))
             (concat [day] inactive-day)))
         sample-dates)))

(defn active-authors-timeline
  [commits options]
  (let [now (time/time-now options)
        active-authors (sort (authors/active-authors commits options))
        sample-dates (sample-dates-for now)
        commits-of-interest (commits-for-past-year commits (a-year-ago-from now))
        contrib-per-day (->> commits-of-interest
                             (group-by :date)
                             (daily-activity-on active-authors sample-dates))]
    {:analysis-result-header (concat ["data"] active-authors)
     :analysis-result-values contrib-per-day}))


