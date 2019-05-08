(ns evolutionary-metrics.analysis.commit-messages
  (:require [evolutionary-metrics.analysis.internal.churn-algos :as churn]))

;;; This module helps you analyze version-control data
;;; based on commit messages.
;;; Our commit messages contain information about our process and
;;; the kind of work we do.
;;; For example, we can use that information to extract
;;; statistics on bug distributions. Note that this data is 
;;; heuristics, not absolute truths (for that you need to mine
;;; your bug tracking system).
;;;
;;; Usage: just provide a regular expression that specifies
;;; the words of interest in :expression-to-match.


(defn- as-word-match-expr
  [raw-expr]
  (re-pattern (str "(?i)" raw-expr)))

(defn match-expr-from
  [options]
  (if-let [mexpr (:expression-to-match options)]
    (as-word-match-expr mexpr)
    (throw
     (IllegalArgumentException.
      "Commit messages: you need to provide an expression to match against."))))

(defn- commit-matches
  "Performs a match for the expression provided by the
   user against a single commit message."
  [mexpr line]
  (re-find mexpr line))

(defn rows-matching-given-expr
  [mexpr ds]
  (filter (comp (fn [m] (commit-matches mexpr m)) :message) ds))

(defn- as-matching-entity-freqs
  [ds]
  (->>
   (map :entity ds)
   frequencies
   (into [])
   (map (fn [v] (zipmap [:entity :matches] v)))))

(defn by-word-frequency
  "Returns the frequencies of the given word matches
   across all entities.
   This analysis is typically used to extrapolate
   bug fixes from the commit messages.
   For example, the user may specify a list of
   suspicious words like bug, error, etc and
   this function counts the occourences."
  [ds options]
  (let [vs (->>
             (rows-matching-given-expr (match-expr-from options) ds)
             as-matching-entity-freqs
             (sort-by (juxt :matches :entity))
             (map (juxt :entity :matches))
             reverse)]
    {:analysis-result-header [:entity :matches]
     :analysis-result-values vs}))

;; Commit message trends
;;

(defn- commits-matching
  [p ds]
  (let [matches? (partial re-find p)]
    (filter (comp (comp some? matches?) :message) ds)))

(defn- commit-statistics-by-date
  [ds-by-date]
  (for [[date-entry commits] ds-by-date
        :let [date date-entry
              revs (churn/revisions-in commits)
              added (churn/total-churn :loc-added commits)]]
    [date revs added]))

(defn trends
  "Returns a dataset with the columns :date :nrevisions :addedcode that
   specifies the number of revisions and code churn that matches a
   particular pattern in the commit messages."
  [ds options]
  (let [vs (->> ds
                (commits-matching (match-expr-from options))
                (group-by :date)
                commit-statistics-by-date
                (sort-by first))]
    {:analysis-result-header [:date :nrevisions :addedcode]
     :analysis-result-values vs}))