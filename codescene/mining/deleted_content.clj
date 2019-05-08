(ns codescene.mining.deleted-content
  (:require [semantic-csv.core :as sc]
            [evolutionary-metrics.mining.file-patterns :as file-patterns]))

(defn make-deletion-date-lookup-by-file-in-repo
  [deletion-log]
  (let [lookuper (->> (sc/slurp-csv deletion-log)
                      (map (juxt :entity :date :rev))
                      (map (fn [[entity date rev]] [(file-patterns/de-scope entity) {:date date :rev rev}]))
                      (into {}))]
    (fn [repo file-name]
      (get lookuper [repo file-name]))))

