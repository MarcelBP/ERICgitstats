(ns hotspots-x-ray.recommendations.developer-congestion
  (:require [evolutionary-metrics.mining.vcs :as vcs]
            [evolutionary-metrics.analysis.effort :as effort]
            [clj-time.core :as tc]
            [evolutionary-metrics.trends.dates :as dates]
            [taoensso.timbre :as log]))

(defn- change-before?
  [cutoff-date change]
  (let [change-date (dates/string->date (:date change))]
    (not (tc/after? change-date cutoff-date))))

(defn- resolve-alias
  [aliases {:keys [author] :as c}]
  (assoc c :author (get aliases author author)))

(defn- fractal-value-of
  [changes]
  (let [adapted-to-effort-form (map (fn [c] (assoc c :entity "The one")) changes)
        {:keys [analysis-result-values]} (effort/as-revisions-based-entity-fragmentation adapted-to-effort-form {})
        [_name fractal-value _total-revs n-authors] (first analysis-result-values)]
    {:authors (or n-authors 0)
     :fractal-value (or fractal-value 0.0)}))

(defn- congestion-stats
  [file-name changes]
  (let [stats (fractal-value-of changes)]
    (log/debug "Developer Congestion for " file-name " : " (:authors stats) " authors, fractal: " (:fractal-value stats))
    stats))

(defn monthly-contributing-devs
  "Calculates the developer congestion for individual developers over the
   past month backwards from time-now."
  [git-cmd repo time-now file-name author-aliases]
  (let [last-month (tc/minus time-now (tc/months 1))
        mining-start-date (dates/date->string last-month)
        cutoff-date (tc/date-time (tc/year time-now) (tc/month time-now) (tc/day time-now))
        changes (vcs/file-versions-with-author-metadata git-cmd repo mining-start-date file-name)]
    (->> changes
         (filter (partial change-before? cutoff-date))
         (map (partial resolve-alias author-aliases))
         (congestion-stats file-name))))
