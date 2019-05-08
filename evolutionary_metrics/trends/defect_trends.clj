(ns evolutionary-metrics.trends.defect-trends
  (:require [clj-time.core :as tc]
            [evolutionary-metrics.trends.dates :as dates]
            [evolutionary-metrics.analysis.commit-messages :as commit-messages]))

(defn- until-now-from
  [t commits]
  (->> commits
       (map (fn [{:keys [date] :as c}]
              (assoc c :date (dates/string->date date))))
       (remove (fn [{:keys [date]}]
                 (tc/before? date t)))))

(defn- keep-commits-with-defects-matching
  [hotspot-defect-commit-pattern commits]
  (commit-messages/rows-matching-given-expr
    (commit-messages/match-expr-from {:expression-to-match hotspot-defect-commit-pattern})
    commits))

(defn- adjust-date-to-start-of-month
  [date]
  (let [yy (tc/year date)
        mm (tc/month date)]
    (tc/date-time yy mm 1)))

(defn- group-by-month
  [commits]
  (->> commits
       (map (fn [{:keys [date] :as c}]
              (let [month-based (adjust-date-to-start-of-month date)]
                (assoc c :date month-based))))
       (group-by :date)))

(defn- defect-freq-by-entity
  [[date commits-for-one-month]]
  [date (->> commits-for-one-month
             (map :entity)
             frequencies)])

(def ^:private months-in-trend 13)

(defn- defects-by-entity-in
  [defects-by-month-and-entity sample-dates entity]
  (->> sample-dates
       (map (fn [d]
              (let [defects-by-entity (get defects-by-month-and-entity d {})]
                (get defects-by-entity entity 0))))
       (concat [entity])))

(defn- with-headers
  [sample-dates]
  (concat ["entity"] (map dates/date->string sample-dates)))

(defn- defect-trend-for
  [start-date entities defects-by-month-and-entity]
  (let [adjusted-start-date (adjust-date-to-start-of-month start-date)
        sample-dates (->> (range months-in-trend) (map (fn [d] (tc/plus adjusted-start-date (tc/months d)))))
        trends-by-entity (map (partial defects-by-entity-in defects-by-month-and-entity sample-dates) entities)]
    {:headers (with-headers sample-dates)
     :trends trends-by-entity}))

(defn defect-trends-by-hotspot
  [{:keys [age-time-now] :as _context} {:keys [hotspot-defect-commit-pattern] :as _project} entities-with-defects commits]
  (let [a-year-ago (tc/minus (dates/string->date age-time-now) (tc/years 1))]
    (->> commits
         (until-now-from a-year-ago)
         (keep-commits-with-defects-matching hotspot-defect-commit-pattern)
         group-by-month
         (map defect-freq-by-entity)
         (into {})
         (defect-trend-for a-year-ago entities-with-defects))))
