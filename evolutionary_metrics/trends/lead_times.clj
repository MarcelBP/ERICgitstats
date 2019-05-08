(ns evolutionary-metrics.trends.lead-times
  (:require [incanter.stats :as stats]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.core :as core]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn- quantiles-of
  [vs]
  (let [[pct5 pct25 pct50 pct75 pct95] (into [] (stats/quantile vs :probs [0.05 0.25 0.5 0.75 0.95]))]
    {:pct05 pct5
     :pct25 pct25
     :pct50 pct50
     :pct75 pct75
     :pct95 pct95}))

(s/def ::ticket-date ::core/date-string)
(s/def ::lead-time nat-int?)
(s/def ::lead-time-by-date (s/keys :req-un [::ticket-date ::lead-time]))
(s/def ::lead-times-by-date (s/coll-of ::lead-time-by-date))

(s/def ::date ::core/date-string)
(s/def ::quantile float?)
(s/def ::pct05 ::quantile)
(s/def ::pct25 ::quantile)
(s/def ::pct50 ::quantile)
(s/def ::pct75 ::quantile)
(s/def ::pct95 ::quantile)

(s/def ::monthly-trend (s/keys :req-un [::date ::pct05 ::pct25 ::pct50 ::pct75 ::pct95]))
(s/def ::lead-time-trends (s/coll-of ::monthly-trend))

(defn- group-by-month
  [vs]
  (->> vs
       (map (fn [{:keys [ticket-date] :as v}]
              (let [[year month _] (clojure.string/split ticket-date #"-")]
                (merge v {:ticket-date (str year "-" month "-01")}))))
       (group-by :ticket-date)))

(s/fdef trend-by-month
        :args (s/cat :lead-times ::lead-times-by-date)
        :ret ::lead-time-trends)

(defn trend-by-month
  [lead-times]
  (let [by-month (group-by-month lead-times)]
    (->> by-month
         (into [])
         (map (fn [[date vs]]
                (merge {:date date}
                       (quantiles-of (map :lead-time vs)))))
         (sort-by :date))))

(s/fdef trend-by-month-to-file
        :args (s/cat :lead-times ::lead-times-by-date
                     :out-file-name (s/? ::core/filename))
        :ret empty?)

(defn trend-by-month-to-file
  "As `trend-by-month but stores output to the file instead of returning it.`"
  [lead-times out-file-name]
  (let [trends (trend-by-month lead-times)
        row-based (map (juxt :date :pct05 :pct25 :pct50 :pct75 :pct95) trends)]
    (with-open [out-file (io/writer out-file-name)]
      (csv/write-csv out-file [["date" "pct05" "pct25" "pct50" "pct75" "pct95"]])
      (csv/write-csv out-file row-based))))
