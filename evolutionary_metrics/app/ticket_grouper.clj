(ns evolutionary-metrics.app.ticket-grouper)

;;; This grouper allows us to analyze temporal coupling accross multiple commits, potentially in different repositories.
;;; We use a Ticket ID, parsed from the commit header, to group individual commits into a single, logical commit that
;;; we then run the analysis on.

(defn- ensure-valid-extraction-pattern
  [pattern]
  (if-not (re-find #"\(.+\)" pattern)
    (throw (IllegalArgumentException. (str "The extraction pattern " pattern " must contain one(1) group that matches the Ticket ID.")))))

(defn- create-ticket-id-extractor
  [message-extraction-pattern]
  (ensure-valid-extraction-pattern message-extraction-pattern) ; blow-up early in case we got an invalid pattern
  (let [pattern (re-pattern message-extraction-pattern)]
    (fn [message]
      (-> (re-find pattern message)
          second ; the first group is ALWAYS our target (documented in the UI)
          (or nil)))))

(defn extract-ticket-id-to-rev
  "Adds a new column :ticked-id to all commits based on the
  message-extraction-pattern."
  [message-extraction-pattern commits]
  (let [extractor (create-ticket-id-extractor message-extraction-pattern)]
    (map (fn [{:keys [message rev] :as c}]
           (-> c
               (assoc :rev (extractor message))
               (assoc :original-rev rev)))
         commits)))

(defn by-ticket-id
  "Groups individual commits into aggregates based on
   the given temporal period and the name of the author. "
  [options commits]
  (let [{:keys [temporal-by-ticket-id message-extraction-pattern]} options]
    (cond
      ;; If both are non-empty strings then let's extract ticket IDs.
      (and temporal-by-ticket-id
           message-extraction-pattern)
      (->> commits
           (extract-ticket-id-to-rev message-extraction-pattern)
           (filter (comp some? :rev)))

      ;; In case just Ticket ID is set, that's an error.
      (and temporal-by-ticket-id (not message-extraction-pattern))
      (throw (IllegalArgumentException. "Please specify both --temporal-by-ticket-id and --message-extraction-pattern."))

      ;; If both are empty or nil we just pass through.
      :else
      commits)))
