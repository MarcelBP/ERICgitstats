(ns evolutionary-metrics.trends.internal.pm-trends)

(defn- slice-day
  "'date' should be a String with format 'yyyy-MM'"
  [date]
  (->> date (re-find #"(\d{4}-\d{2})-\d+") second))

(defn- slice-days
  "We want to aggregate by month, so let's
   slice away the day field"
  [revs]
  (map (fn [[rev date]] [rev (slice-day date)]) revs))

(defn- filter-latest-date-for-rev
  [revs-by-date]
  (->> revs-by-date
       (sort-by second)
       (into {}) ; NOTE: won't work unless the seq is sorted on date!!
       (into [])))

(defn unique-timestamped-revisions-in ; since the Ticket IDs may be duplicated and we only want to calculate each Ticket cost once per entity
  [ds]
  (->> ds
       (map (juxt :rev :date))
       slice-days
       distinct
       filter-latest-date-for-rev
       (sort-by second)))

(defn- revs-by-entity
  [grouped]
  (for [[entity changes] grouped
        :let [entity-name entity
              revisions (unique-timestamped-revisions-in changes)]]
    [entity-name revisions]))

(defn as-commits-by-entity
  [commits entities]
  {:pre  [(set? entities)]}
  (->> commits
       (filter (comp entities :entity))
       (group-by :entity)
       revs-by-entity
       (into {})))
