(ns evolutionary-metrics.app.periodical-trends
  (:require [evolutionary-metrics.trends.dates :as dates]
            [clj-time.core :as tc]))

(defn- sunday-in-the-week-of
  [date]
  (let [day-of-week (tc/day-of-week date)
        days-left (max (- 7 day-of-week) 0)]
    (tc/plus date (tc/days days-left))))

(defn- adjust-to-weekly-date
  [{:keys [date] :as c}]
  (let [d (dates/string->date date)
        trend-date (sunday-in-the-week-of d)]
    (assoc c :date (dates/date->string trend-date))))

(defn by-weekly-commits
  "Groups the commits by week by adjusting the commit date in
   the provided dataset.
   This is typically used to avoid steep spikes and note clear patterns
   in long-term trends."
  [options commits]
  (if (:by-weekly-trend options)
    (map adjust-to-weekly-date commits)
    commits))
