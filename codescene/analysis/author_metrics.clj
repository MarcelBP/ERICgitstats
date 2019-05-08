(ns codescene.analysis.author-metrics
  (:require [evolutionary-metrics.trends.authors-trend :as authors-trend]
            [evolutionary-metrics.trends.dates :as dates]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [clj-time.core :as tc]
            [codescene.analysis.paths :as paths]))

(defn- author-churn->contribution-spans
  [churn-file]
  (->> (shared/read-csv-sans-header-from churn-file)
       (map (fn [row] [(nth row 7) (nth row 6)]))
       (map (fn [[start-date end-date]]
              {:start-date (dates/string->date start-date)
               :end-date (dates/string->date end-date)}))))

(defn last-contribution-date
  "We may analyse a historic codebase without recent activity. Thus, we need
   to pick the last known contribution date rather than the current time which
   may be far in the future compared to the last development activity."
  [churn]
  (->> churn
       (map :end-date)
       sort
       last))

(defn contributors-per-month
  [{:keys [analysis-path-fn time-now] :as _context}
   {:keys [analysis-start-date] :as _project}
   out-file-name]
  (let [author-churn-file (analysis-path-fn paths/author-churn-csv)
        contributions (author-churn->contribution-spans author-churn-file)]
    (if (seq contributions)
      (let [end-date (or (last-contribution-date contributions) time-now)
            analysis-interval (tc/interval (dates/string->date analysis-start-date) end-date)
            destination (analysis-path-fn out-file-name)]
        (authors-trend/persist-contributing-authors-per-month analysis-interval contributions destination))
      []))) ; no authors
