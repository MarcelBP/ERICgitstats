(ns codescene.analysis.pair-programming-configurer)

(defn enable-pair-programming-when-specified
  [{:keys [pair-programming-pattern] :as _project} analysis-params]
  (if (seq pair-programming-pattern)
    (merge {:pair-programming-pattern pair-programming-pattern} analysis-params)
    analysis-params))
