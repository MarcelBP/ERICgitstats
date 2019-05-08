(ns evolutionary-metrics.analysis.ticket-id
  (:require [clojure.spec.alpha :as s]
            [evolutionary-metrics.parsers :as parsers]
            [evolutionary-metrics.analysis.internal.churn-algos :as c]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.trends.dates :as dates]
            [clj-time.core :as tc]))

(defn- lead-time-in-hours
  [start-date stop-date]
  (try
    (max (tc/in-hours (tc/interval start-date stop-date))
         1)
    (catch Exception e
      (throw (IllegalArgumentException. (str "Invalid interval for lead time. Start = " start-date ", Stop = " stop-date), e)))))

(defn- statistics-for-ticket-id-group
  [group-entry changes]
  (let [ticket group-entry
        modification-dates (->> changes (map :date) sort)
        detailed-dates (->> changes (map :basic-date-time) (map dates/date-time-string->date) sort)
        stop-date (last modification-dates)
        start-date (first modification-dates)
        detailed-stop-date (last detailed-dates)
        detailed-start-date (first detailed-dates)
        lead-time (lead-time-in-hours detailed-start-date detailed-stop-date)
        added (c/total-churn :loc-added changes)
        deleted (c/total-churn :loc-deleted changes)
        net (- added deleted)
        files (c/count-distinct-in :entity changes)
        authors (c/count-distinct-in :author changes)]
    {:date stop-date
     :ticket ticket
     :added added
     :deleted deleted
     :net net
     :files files
     :authors authors
     :startdate start-date
     :leadtime lead-time}))

(defn- sum-by-ticket-id
  [grouped-by-ticket]
  (map (fn [[group-entry changes]] (statistics-for-ticket-id-group group-entry changes)) grouped-by-ticket))

(s/fdef absolute-by-ticket-id
        :args (s/cat :ds ::parsers/vcs-log
                     :options ::core/options)
        :ret  ::core/analysis-result)

(defn absolute-by-ticket-id
  [commits _options]
  (let [vs (->> commits
                (group-by :rev)
                sum-by-ticket-id
                (sort-by (juxt :date :ticket :added))
                (map (juxt :date :ticket :added :deleted :net :files :authors :startdate :leadtime)))]
    {:analysis-result-header [:date :ticket :added :deleted :net :files :authors :startdate :leadtime]
     :analysis-result-values vs}))
