(ns evolutionary-metrics.analysis.revision-churn
  (:require [evolutionary-metrics.analysis.internal.churn-algos :as churn]
            [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [clj-time.core :as tc]
            [evolutionary-metrics.trends.dates :as dates]))

(defn- count-unique
  [selector ds]
  (->> (map selector ds)
       distinct
       count
       (max 1)))

(defn- sunday-in-the-week-of
  [date]
  (let [day-of-week (tc/day-of-week date)
        days-left (max (- 7 day-of-week) 0)]
    (tc/plus date (tc/days days-left))))

(defn- churn-statistics-by-date
  "Calculates the churn statistics with respect to
   the number of revisions and authors per day"
  [grouped]
  (for [[group-entry changes] grouped
        :let [group-date group-entry
              revisions (count-unique :rev changes)
              authors (count-unique :author changes)]]
    {:date group-date :revisions revisions :authors authors}))

(defn- adjust-to-weekly-date
  [{:keys [date] :as c}]
  (let [d (dates/string->date date)
        trend-date (sunday-in-the-week-of d)]
    (assoc c :date trend-date)))

(defn- date-output-format
  [{:keys [date] :as c}]
  (assoc c :date (dates/date->string date)))

(defn- date-in-the-future-of
  "Since we create a trend where the commits are aggregated on a weekly basis, we
   could get a steep dip in the last, ongoing week. This will be misleading in the
   analysis so we simply filter away the final incomplete week."
  [age-time-now {:keys [date] :as _commit}]
  (tc/after? date age-time-now))

(defn- date-filter-for
  [age-time-now]
  (if (some? age-time-now)
    (partial date-in-the-future-of (dates/string->date age-time-now))
    (constantly false)))

(defn revisions-by-date
  "Calculates an Incanter dataset with the number of commits
   each week. Note that only dates with commits are considered."
  [commits {:keys [age-time-now] :as _options}]
  (let [dates-beyond-trend-cut-off (date-filter-for age-time-now)
        vs (->> commits
                (map adjust-to-weekly-date)
                (remove dates-beyond-trend-cut-off)
                (map date-output-format)
                (group-by :date)
                churn-statistics-by-date
                (sort-by (juxt :date :revisions :authors))
                (map (juxt :date :revisions :authors)))]
    {:analysis-result-header [:date :revisions :authors]
     :analysis-result-values vs}))

                  (defn- pick-one-for
  [changes selector]
  (->> changes (map selector) first))

(def ^:private repository-from file-patterns/first-segment)

(defn- non-zero-net-churn?
  "If a developer makes a large, sweeping rename, CodeScene will flag it as a high risk commit.
  This is unnecessary as we can detect the pattern. The rule should simply be to filter away all
  commits where *every* modified file has the same number of added/deleted lines."
  [changes-in-commit]
  (->> changes-in-commit
       (map (juxt :loc-added :loc-deleted))
       (every? (fn [[added deleted]] (= added deleted)))
       false?))

(def ^:private obvious-rename-threshold 3) ; files

(defn revision-contains-logic-changes?
  [changes-in-commit]
  (if (> (count changes-in-commit) obvious-rename-threshold)
    (non-zero-net-churn? changes-in-commit)
    true))

(defn- diffusion-by-rev
  [grouped-by-rev]
  (for [[group-entry changes] grouped-by-rev
        :let [rev group-entry
              date (pick-one-for changes :date)
              author (pick-one-for changes :author)
              files (count-unique :entity changes)
              sub-systems 0  ; TODO: we need the transformations here!
              added-code (churn/total-churn :loc-added changes)
              repository (repository-from (pick-one-for changes :entity))]
        :when (revision-contains-logic-changes? changes)]
    [rev date author files sub-systems added-code repository]))

(defn- remove-move-only-commits
  [commits]
  (filter (fn [{:keys [loc-added loc-deleted]}]
            (not (and (= loc-added "0")
                      (= loc-deleted "0"))))
          commits))

(defn diffusion-by-revision
  "Calculates the number of modified files and, optionally, the
   number of modified sub-systems.
   These metrics are typically used as part of our risk classification."
  [commits _options]
  (let [reverse-compare #(compare %2 %1)
        vs (->> commits
                remove-move-only-commits
                (group-by :rev)
                diffusion-by-rev
                (sort-by (juxt second first) reverse-compare))]
    {:analysis-result-header [:rev :date :author :files :subsystems :addedcode :repository]
     :analysis-result-values vs}))
