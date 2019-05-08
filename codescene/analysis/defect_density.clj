(ns codescene.analysis.defect-density
  (:require [codescene.analysis.closed-maat-proxy :as maat]
            [codescene.analysis.paths :as paths]
            [evolutionary-metrics.trends.defect-trends :as trends]
            [semantic-csv.core :as sc]
            [taoensso.timbre :as log]
            [evolutionary-metrics.analysis.math :as math]
            [evolutionary-metrics.analysis.commit-messages :as cm]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(defn- valid-pattern-to-match?
  [p]
  (and (some? p)
       (not (empty? p))))

(defn- analyse-messages-matching
  [hotspot-defect-commit-pattern {:keys [filtered-log code-maat-params]} destination]
  (let [analysis-params (merge code-maat-params {:expression-to-match hotspot-defect-commit-pattern})]
    (log/info "Calculating defect density using the pattern " hotspot-defect-commit-pattern)
    (maat/run-closed-maat filtered-log "messages" destination analysis-params)))

(defn- generate-empty-result-file-for-defects
  "Let us avoid special cases down the line."
  [destination]
  (log/info "Skipping defect density analysis since the bug-matching pattern is absent from project configuration")
  (with-open [out-file (io/writer destination)]
    (csv/write-csv out-file [["entity" "matches"]])))

(defn defects-by-file
  [{:keys [analysis-path-fn] :as project} {:keys [hotspot-defect-commit-pattern]} dest-file-name]
  (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))]
    (if (valid-pattern-to-match? hotspot-defect-commit-pattern)
      (analyse-messages-matching hotspot-defect-commit-pattern project destination)
      (generate-empty-result-file-for-defects destination))))

(defn- ->revisions-by-name
  [revisions-csv]
  (->> (sc/slurp-csv revisions-csv)
       (sc/cast-with {:n-revs sc/->int})
       (map (juxt :entity :n-revs))
       (into {})))

(defn- ->density
  [density-csv]
  (->> (sc/slurp-csv density-csv)
       (sc/cast-with {:matches sc/->int})
       (map (juxt :entity :matches))))

(defn- density-of
  [n-defects n-revisions]
  (if (> n-revisions 0)
    (int (math/as-percentage  (/ n-defects n-revisions)))
    0))

(defn- add-density-to
  [defects revisions-lookup]
  (for [[name n-defects] defects
        :let [matching-revs (revisions-lookup name 0)]]
    {:name name
     :density (density-of n-defects matching-revs)
     :defects n-defects}))

(defn density-by-max-defects
  "Let the density be a relative value defined as the percentage of bug fixes relative
   to the total number of revisions for the hotpot.
   Include the total number of defects per hotspot as an absolute value; Absolute values
   are better for ranking as they are less sensitive to outliers (e.g. a file with one
   commit that is a bug fix = 100% density)."
  [defects-csv revisions-csv destination-file]
  (let [defects (->density defects-csv)
        revisions-lookup (->revisions-by-name revisions-csv)
        density-by-file (add-density-to defects revisions-lookup)]
    (sc/spit-csv destination-file density-by-file)))

(defn defect-trends-by-hotspot
  "Generates the defect distributions for each month over the past year on a per
   hotspot basis. This data lets a user inspect the trend to see if a cluster of
   bugs in a file is recent or happended 6 months ago."
  [{:keys [filtered-log] :as context} {:keys [hotspot-defect-commit-pattern] :as project} defects-csv destination-file]
  (when (valid-pattern-to-match? hotspot-defect-commit-pattern)
    (let [commits (sc/slurp-csv filtered-log)
          entities (->> defects-csv sc/slurp-csv (map :entity))
          {:keys [headers trends]} (trends/defect-trends-by-hotspot context project entities commits)]
      (with-open [out-file (io/writer destination-file)]
        (csv/write-csv out-file [headers])
        (csv/write-csv out-file trends)))))

;; Statistics for the dashboard
;;

(defn- commits-with-defects
  [matcher commits]
  (->> commits
       (cm/rows-matching-given-expr matcher)
       (group-by :rev)))

(defn- plus-one-for-match
  [hotspots commit]
  (let [cfs (->> commit (map :entity) set)
        matched-hotspots (clojure.set/intersection cfs hotspots)]
    (if (empty? matched-hotspots)
      0
      1)))

(defn- hotspot-count-in
  [grouped-commits-with-defects hotspots]
  (->> grouped-commits-with-defects
       vals
       (map (partial plus-one-for-match (set hotspots)))
       (reduce +)))

(defn- unique-defects-in-commits
  [defect-commits]
  (-> defect-commits keys count))

(defn defect-statistics-for-hotspots
  [{:keys [hotspot-defect-commit-pattern] :as _project} commits hotspots]
  (if (valid-pattern-to-match? hotspot-defect-commit-pattern)
    (let [matcher (cm/match-expr-from {:expression-to-match hotspot-defect-commit-pattern})
          defect-commits (commits-with-defects matcher commits)
          hotspot-matches (hotspot-count-in defect-commits hotspots)]
      {:unique-defects (unique-defects-in-commits defect-commits)
       :defects-in-hotspots hotspot-matches})
    {:unique-defects 0
     :defects-in-hotspots 0}))

;; Architectural analyses
;;

(def ^:const no-defect-pattern-configured "-")

(defn defect-statistics-lookup-for-architectural-components
  "The given arch-level-commits is the architectural log where each :entity is an
   architectural component. This function returns a closure that can be queried for the
   number of defects per component."
  [{:keys [hotspot-defect-commit-pattern] :as _project} arch-level-commits]
  (if (valid-pattern-to-match? hotspot-defect-commit-pattern)
    (let [matcher (cm/match-expr-from {:expression-to-match hotspot-defect-commit-pattern})
          component-lookup (->> arch-level-commits
                                (group-by :entity)
                                (map (fn [[component-name cs]]
                                       [component-name
                                        (->> cs
                                             (commits-with-defects matcher)
                                             unique-defects-in-commits)]))
                                (into {}))]
      (fn [name]
        (get component-lookup name no-defect-pattern-configured)))
    (constantly no-defect-pattern-configured)))


