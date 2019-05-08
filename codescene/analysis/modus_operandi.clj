(ns codescene.analysis.modus-operandi
  (:require [codescene.analysis.closed-maat-proxy :as maat]
            [codescene.analysis.paths :as paths]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [taoensso.timbre :as log]))

(def ^:private trend-analysis "modus-commit-trend")

(defn- parts-in
  [{:keys [modus-commit-message-pattern] :as _project}]
  (clojure.string/split modus-commit-message-pattern #"\|"))

(defn- generate-sub-trends?
  [{:keys [split-modus-commit-graphs] :as project}]
  (and (true? split-modus-commit-graphs)
       (< 1 (count (parts-in project)))))

(defn- commit-message-trend
  [{:keys [evo-log analysis-path-fn code-maat-params]} commit-message-pattern dest-file-name]
  (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))
        analysis-params (merge code-maat-params {:expression-to-match commit-message-pattern
                                                 :by-weekly-trend true})]
    (maat/run-closed-maat evo-log trend-analysis destination analysis-params)))

(defn- trend-for-date
  [patterns [date trends]]
  (let [lookupable (->> trends
                        (map (fn [{:keys [pattern nrevisions]}]
                          {pattern nrevisions}))
                        (into {}))]
    (flatten [date (map (fn [p]
                          (get lookupable p 0))
                        patterns)])))

(defn- group-parts
  [patterns parts]
  (->> parts
       flatten
       (group-by :date)
       (map (partial trend-for-date patterns))
       (sort-by first)))

(defn- generate-sub-trends-for
  [{:keys [evo-log analysis-path-fn code-maat-params]} project]
  (when (generate-sub-trends? project)
    (try
      (let [parts (parts-in project)
            trends (->> parts
                        (mapv (fn [p]
                                (let [analysis-params (merge code-maat-params {:expression-to-match p
                                                                               :by-weekly-trend true
                                                                               :identity-output true})
                                      dummy-dest (analysis-path-fn paths/modus-commit-message-partial-path)
                                      res (maat/run-closed-maat evo-log trend-analysis dummy-dest analysis-params)]
                                  (->> (:analysis-result-values res)
                                       (map (fn [[date nrevs _nloc]]
                                              {:date date :pattern p :nrevisions nrevs}))))))
                        (group-parts parts))]
        (with-open [out-file (io/writer (analysis-path-fn paths/modus-commit-message-sub-trends-csv))]
          (csv/write-csv out-file [(into ["date"] parts)])
          (csv/write-csv out-file trends)))
      (catch Throwable e
        (log/error "Modus operandi: cannot generate sub-trends for the patterns " (:modus-commit-message-pattern project) " -- disabling sub-trends in this analysis." e)))))

(defn analyze-commit-message-trend
  [context {:keys [modus-commit-message-pattern] :as project} dest-file-name]
  (when (some? modus-commit-message-pattern)
    (commit-message-trend context modus-commit-message-pattern dest-file-name)
    (generate-sub-trends-for context project)))
