(ns evolutionary-metrics.app.date-filter
  (:require [clj-time.core :as t]
            [evolutionary-metrics.parsers.time-parser :as tp]))

(def ^:private as-common-time-object (tp/time-parser-from tp/internal-time-format-string))

(defn- start-date-keeper
  [start-date]
  (if start-date
    #(not (t/before? % start-date))
    (constantly true)))

(defn- end-date-keeper
  [end-date]
  (if end-date
    #(not (t/after? % end-date))
    (constantly true)))

(defn- make-date-filter-from
  [start-date end-date]
  (let [after-start-date? (start-date-keeper start-date)
        before-end-date? (end-date-keeper end-date)]
    (fn [date]
      (let [entry-date (as-common-time-object date)]
        (and (after-start-date? entry-date) (before-end-date? entry-date))))))

(defn- remove-from
  [keep? commits]
  (filter (comp keep? :date) commits))

(defn- filter-files-outside
  [start-date end-date commits]
  (remove-from (make-date-filter-from start-date end-date) commits))

(defn by-date-range
  "Filters the log by explicit dates provided by the user.

   The git log is typically filtered during the data mining, but we may have
   to clean it here. The reason is that we mine the log based on commit dates, but
   those may include earlier author dates. Often, that's just the behavior we want.
   But sometimes, for example when exploring social metrics, we want to get rid of
   that earlier history.

   Another use case is when we do trend analyses, for example looking back at the
   hotspots a week ago."
  [{:keys [filter-start-date filter-end-date]} commits]
  (if (or filter-start-date filter-end-date)
    (filter-files-outside filter-start-date filter-end-date commits)
    commits))
