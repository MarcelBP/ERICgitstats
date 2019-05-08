(ns evolutionary-metrics.analysis.identity-analysis)

(defn identity-dataset
  [ds _options]
  {:analysis-result-header [:author :rev :date :entity :message :loc-added :loc-deleted :author-email :basic-date-time]
   :analysis-result-values (map (juxt :author :rev :date :entity :message :loc-added :loc-deleted :author-email :basic-date-time) ds)})
