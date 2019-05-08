(ns codescene.license.license-criterias
  (:require [clj-time.core :as tc]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.trends.dates :as dates]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [codescene.analysis.author-metrics :as m]
            [codescene.analysis.paths :as paths]
            [codescene.analysis.specs :as analysis-specs]))

(defn author-churn->last-contribution
  [churn-file]
  (->> (shared/read-csv-sans-header-from churn-file)
       (map (fn [row] [(nth row 0) (nth row 6)]))
       (map (fn [[name end-date]]
              {:name name
               :end-date (dates/string->date end-date)}))))

(s/def ::existing-path (s/and string? #(.exists (io/as-file %))))

(s/def ::active-author-names (s/coll-of string?))

(s/fdef active-authors-in
        :args (s/cat :analysis-path ::existing-path
                     :cut-off-age-for-active-author ::analysis-specs/time-period)
        :ret ::active-author-names)

(defn active-authors-in
  "Returns a set with all authors that have made contributions
  in the past months (as specified by cut-off-age-for-active-author).
  The information is fetched from the given analysis."
  [analysis-path cut-off-age-for-active-author]
  (let [author-churn-file (paths/as-child-path analysis-path paths/author-churn-csv)]
    (if-let [contributions (seq (author-churn->last-contribution author-churn-file))]
      (let [end-date (m/last-contribution-date contributions)
            cut-off (tc/minus end-date cut-off-age-for-active-author)]
        (->> contributions
             (remove (comp (partial tc/after? cut-off) :end-date))
             (map :name)
             (into #{})))
      #{})))

