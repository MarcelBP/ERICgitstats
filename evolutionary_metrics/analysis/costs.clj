(ns evolutionary-metrics.analysis.costs
  (:require [evolutionary-metrics.mergers.shared-mergers :as shared]
            [medley.core :as m]))

(defn- unique-revisions-in ; since the Ticket IDs may be duplicated and we only want to calculate each Ticket cost once per entity
  [ds]
  (->> ds (map :rev) distinct))

(defn- total-cost-of-revisions
  [costs-by-id revisions]
  (reduce (fn [acc id]
            (+ acc (get costs-by-id id 0)))
          0
          revisions))

(defn- cost-statistics-by-entity
  [costs-by-id grouped]
  (for [[entity changes] grouped
        :let [entity-name entity
              revisions (unique-revisions-in changes)
              cost (total-cost-of-revisions costs-by-id revisions)]]
    [entity-name cost]))

(defn as-costs-by-id
  [costs-log-name]
  (->> (shared/read-csv-sans-header-from costs-log-name)
       (map (fn [[id cost]] [id (Integer/parseInt cost)]))
       (into {})))

(defn costs-of-entity-by-id-lookup
  [commits costs-by-id]
  (let [vs (->> commits
                (group-by :entity)
                (cost-statistics-by-entity costs-by-id)
                (sort-by second)
                reverse)]
    {:analysis-result-header [:entity :cost]
     :analysis-result-values vs}))

(defn costs-of
  "Calculates an Incanter dataset with the costs of development work
   per entity. The costs are calculated by summing up the project
   management data provided in the referenced file, which contains a
   mapping from the revision (may be a Ticket ID) to time spent."
  [commits {:keys [costs-log-name]}]
  (costs-of-entity-by-id-lookup commits (as-costs-by-id costs-log-name)))
