(ns evolutionary-metrics.app.pair-program-knowledge-mapper
  (:require [evolutionary-metrics.analysis.internal.churn-algos :as churn-algos]))

;; We use a simple algorithm for pair programming:
;; - The pairs are extracted either from the commit message.
;; - To support mob programming, we allow any number of authors (one per match group).
;; - If any authors are defined for a commit we split the contributions equally between them.
;; - The way we "split" contributions is by duplicating the commit.
;; - If the code churn cannot be evenly split, it's rounded up to the nearest integer.
;;   That is, a code churn of '1' gives each author a contribution of '1' LoC.
;;   A code churn of '5' gives each author a contribution of '3' LoC.
;;   This is OK since it's "only" used to reflect the parts of the code where they
;;   have worked relative to each other.

(defn- shared-churn
  [v n-authors]
  (-> v
      churn-algos/as-int
      (/ (float n-authors))
      Math/ceil
      Math/round
      str)) ; maintain the (historic) contract to the other filters in the pipe

(defn- split-churn-between
  [authors {:keys [rev loc-added loc-deleted] :as commit}]
  (let [n-authors (count authors)
        added (shared-churn loc-added n-authors)
        deleted (shared-churn loc-deleted n-authors)]
    (for [[n a] (map-indexed vector authors)
          :let [synthetic-rev (str rev (inc n))
                c (assoc commit :rev synthetic-rev :loc-added added :loc-deleted deleted :author a)]
          :when (seq a)]
      c)))

(defn- split-commit-by-authors
  [pp-exp {:keys [message] :as commit}]
  (if-let [additional-authors (re-find pp-exp message)]
      (split-churn-between (->> additional-authors (drop 1) (filter seq))
                           commit)
    [commit]))

(defn- split-contributions
  [pair-programming-pattern commits]
  (let [pp-exp (re-pattern pair-programming-pattern)]
    (mapcat (partial split-commit-by-authors pp-exp) commits)))

(defn resolve-contributors
  "Splits the contributions between the pairs, if any."
  [{:keys [pair-programming-pattern] :as _options} commits]
  (if (seq pair-programming-pattern)
    (split-contributions pair-programming-pattern commits)
    commits))
