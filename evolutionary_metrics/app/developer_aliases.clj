(ns evolutionary-metrics.app.developer-aliases)

(defn resolve-authors
  "Translates the author names found in the Git log to names resolved by the user."
  [{:keys [developer-alias-map] :as _options} commits]
  (if (seq developer-alias-map)
    (map (fn [{:keys [author] :as change}]
           (assoc change :author (get developer-alias-map author author)))
         commits)
    commits))

