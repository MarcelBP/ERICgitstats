(ns evolutionary-metrics.analysis.author-churn
  (:require [evolutionary-metrics.analysis.internal.churn-algos :as c]
            [evolutionary-metrics.analysis.internal.authors-shared :as authors-shared]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.parsers :as parsers]
            [clojure.spec.alpha :as s]))

(defn- contribution-time-statistics
  [grouped]
  (for [[group-entry changes] grouped
        :let [churn-statistics (c/statistics-for-group-entry group-entry changes)
              time-statistics (authors-shared/contribution-time changes)]]
    (concat churn-statistics time-statistics)))

(def ^:const headers [:author :added :deleted :net :revisions :months :lastcontrib :firstcontrib])

(defn- author-statistics
  [ds]
  (->>
    (group-by :author ds)
    contribution-time-statistics
    (map (fn [vs] (zipmap headers vs)))))

(s/fdef by-author
        :args (s/cat :commits ::parsers/vcs-log
                     :options map?)
        :ret ::core/analysis-result)

(defn by-author
  "Sums the total churn for each contributing author."
  [commits _options]
  (let [vs (->> commits
                author-statistics
                (sort-by (juxt :added :net :deleted))
                (map (juxt :author :added :deleted :net :revisions :months :lastcontrib :firstcontrib))
                reverse)]
    {:analysis-result-header headers
     :analysis-result-values vs}))
