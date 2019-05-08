(ns codescene.presentation.hotspot-health
  (:require
   [codescene.analysis.paths :as paths]
   [codescene.presentation.health :as health]
   [codescene.presentation.display :as display]
   [evolutionary-metrics.mining.file-patterns :as file-patterns]
   [clojure.set :as set]))

(def  ^:private renames
  {:score :current-score
   :last-month :month-score
   :last-year :year-score})

(defn path->name-and-path
  [row]
  (let [max-path-length 60]
    (-> row
        (assoc :filename (file-patterns/final-segment (:name row)))
        (assoc :path (-> row
                         :name
                         file-patterns/except-final-segment
                         (health/shorten-path max-path-length))))))

(defn- select-temporal-scope
  "In case of incorrect scope, assumes `month`."
  [row scope]
  (assoc row :previous-score (if (= scope "year")
                               (:year-score row)
                               (:month-score row))))

(defn- hotspot-statuses
  "Uses `:previous-score`, must be called after `select-temporal-scope`."
  [{:keys [current-score previous-score] :as row}]
  (assoc row
         :current-score-status (health/val->status current-score 3 8)
         :previous-score-status (health/val->status previous-score 3 8)))

(defn- present-hotspot?
  "CodeScene cannot calculate biomarkers for all hotspots (calculated).
   We also don't want to show non-deterministic prioritized hotspots (presentable)."
  [{:keys [calculated presentable] :as s}]
  (and (= "true" calculated)
       (= "1" presentable)))

(defn formatted-hotspot-health
  [code-marker-rows temporal-scope]
  (->> code-marker-rows
       (filter present-hotspot?)
       (map
         (fn [row]
           (-> row
               display/to-ints-when-possible
               (set/rename-keys renames)
               path->name-and-path
               (select-temporal-scope temporal-scope)
               hotspot-statuses)))))

