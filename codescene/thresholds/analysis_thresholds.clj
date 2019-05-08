(ns codescene.thresholds.analysis-thresholds
  (:require [evolutionary-metrics.mergers.shared-mergers :as shared]
            [codescene.stats.math :as math]))

;; The configuration options in older versions of CodeScene were quite overwhelming.
;; In particular, the  thresholds for the early warnings were really, really hard to get right.
;; As a consequence, many users didn't get the warnings we could have delivered.
;; The code in this file provides a new approach by auto-calculating thresholds based
;; on how the activity and code looks in a project under analysis.
;;
;; NOTE: filtering outliers won't necessarily give us better statistics.
;; Instead we base our thresholds on median values and provide some sensible
;; lower boundaries.

(defn- n-hotspots-of-interest
  [n-files]
  (cond
    (< n-files 100)  10
    (< n-files 3000) 50
    (< n-files 5000) 100
    :else 200))

(defn- parse-hotspots
  [hotspot-csv-file]
  (map (fn [[_name revs loc]] {:revisions (Integer/parseInt revs)
                               :loc (Integer/parseInt loc)})
       (shared/read-csv-sans-header-from hotspot-csv-file)))

(defn describe-hotspots
  [hotspot-csv-file]
  (let [hotspots (parse-hotspots hotspot-csv-file)
        revs (map :revisions hotspots)
        locs (map :loc hotspots)]
    {:files     (count hotspots)
     :revisions (math/stats-for revs)
     :loc       (math/stats-for locs)}))

(defn- round [^Double d]
  (Math/round d))

(defn- minimum-revisions-for-warning
  [{:keys [revisions]}]
  (max (round (:median revisions)) 10))

(defn rising-hotspots-warning
  [{:keys [files loc] :as hotspot-stats}]
  {:min-revisions     (minimum-revisions-for-warning hotspot-stats)
   :min-new-rank      (n-hotspots-of-interest files)
   :min-rank-increase 10
   :min-code-size     (max (round (:median loc)) 100)})

(defn- n-complexity-trends-of-interest
  [n-files]
  (cond
    (< n-files  5000) 100
    (< n-files  7000) 200
    :else 300))

(defn complexity-trend-warning-thresholds
  [{:keys [files loc] :as hotspot-stats}]
  {:n-complexity-trends    (n-complexity-trends-of-interest files)
   :threshold-in-sd-yellow 1
   :min-percentage-yellow  0.1
   :threshold-in-sd-red    2
   :min-percentage-red     0.25
   :trend-period-in-months 6
   :min-sample-points      (minimum-revisions-for-warning hotspot-stats)
   :min-loc                (max (round (:mean loc)) 200)})

