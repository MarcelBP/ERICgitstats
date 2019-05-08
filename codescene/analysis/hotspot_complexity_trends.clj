(ns codescene.analysis.hotspot-complexity-trends
  (:require [evolutionary-metrics.mergers.shared-mergers :as shared]
            [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [evolutionary-metrics.trends.complexity-trend :as complexity]
            [evolutionary-metrics.trends.complexity-trend-step-detection :as complexity-trend-step-detection]
            [codescene.analysis.paths :as paths]
            [codescene.mining.deleted-content :as deleted-content]
            [clojure.java.io :as io]
            [digest :as digest]
            [taoensso.timbre :as log]
            [evolutionary-metrics.mergers.hotspots :as hotspots]
            [taoensso.timbre :as timbre]
            [codescene.analysis.closed-maat-proxy :as maat]
            [evolutionary-metrics.mining.vcs :as vcs])
  (:import (java.io File)))


(defn- indexed-hotspots-in-analysis
  [hotspots-log n-spots-to-trend-analyze]
  (let [all-spots (shared/read-csv-sans-header-from hotspots-log)
        spots-to-consider (min n-spots-to-trend-analyze (count all-spots))]
    (map-indexed (fn [i [name _ _]] [name i])
                 (take spots-to-consider all-spots))))

(defn- complexity-trend-path-by-index
  "Calculate the complexity trend file path from it's index in the hotspots CSV
  file.

  NOTE: This is the legacy way of building the path."
  [{:keys [analysis-result-directory qualified-name n-spots-to-trend-analyze]}]
  (let [spots (indexed-hotspots-in-analysis (paths/make-analysis-path-to
                                             analysis-result-directory
                                             paths/hotspots-csv)
                                            n-spots-to-trend-analyze)
        spots-lookup (into {} spots)
        spot-index (get spots-lookup qualified-name)]
    (when spot-index
      (paths/make-analysis-path-to analysis-result-directory
                                   (paths/spot-index->trend-file-name spot-index)))))

(def ^:const destination-folder "complexity-trends")

(defn- complexity-trend-path-by-hash
  "Calculate the complexity trend file path by hashing the repository-qualified
  file name. Returns paths for files that does not exist as well."
  [{:keys [analysis-result-directory qualified-name]}]
  (->> (io/file (str (digest/sha-1 qualified-name) ".csv"))
       (io/file destination-folder)
       (io/file analysis-result-directory)
       .getPath))

(defn complexity-trend-path
  [version options]
  (cond
    (= version 1) (complexity-trend-path-by-index options)
    :else (complexity-trend-path-by-hash options)))

(defn- complexity-trend-paths-for-hotspots
  [{:keys [trend-filter hotspot-input]} version analysis-result-directory]
  (->> (shared/read-csv-sans-header-from hotspot-input)
       trend-filter
       (filter (comp complexity/language-rules-for first))
       (map (fn [[name revisions _]]
              (let [p (complexity-trend-path version {:analysis-result-directory analysis-result-directory
                                                      :qualified-name name
                                                      :n-spots-to-trend-analyze 0 ;; Deprecated, so we don't need this for new analysis runs.
                                                      })]
                {:hotspot-name name
                 :hotspot-revisions (Integer/parseInt revisions)
                 :filename p})))))

(defn- exclude-commits-by-repo-lookup
  "The creation of the filter is quite expensive since we need to query Git.
   That's why we build up an lookup structure for each repo as we don't know to
   which the exclusions apply."
  [git-client repo-paths exclude-commits]
  (if-not (seq exclude-commits)
    (constantly false) ; do not exclude
    (let [by-repo (->> repo-paths
                       (map (fn [repo-path]
                              [repo-path
                               (->> exclude-commits
                                    (map (partial vcs/to-short-hash git-client repo-path))
                                    set)]))
                       (into {}))]
      (fn [repo commit]
        (if-let [r (get by-repo repo)]
          (r commit)
          false)))))

(defn make-trend-calculator
  [scope->repo-lookup analysis-start-date scrambled-translation calculate-trend-fn]
  (fn [{:keys [hotspot-name filename]}]
    (let [unscrambled-name (get scrambled-translation hotspot-name hotspot-name)
          [scope real-file-name] (file-patterns/de-scope unscrambled-name)
          repo-path (scope->repo-lookup scope)
          trend-fn (partial calculate-trend-fn scope repo-path analysis-start-date real-file-name filename)]
      (when (nil? repo-path)
        (log/error "Trends: failed to resolve the repository for '" unscrambled-name "' which resolved to '"
                   real-file-name "' with repositories = "
                   (keys scope->repo-lookup)
                   " using path separator: " File/separator) ; leaky, but for debugging
        (throw (IllegalArgumentException. (str "Failed to calculate complexity trend for '" unscrambled-name "'"))))
      (io/make-parents filename)
      (try
        (trend-fn)
        (catch Exception e
          (log/info "Failed first attempt at calculating trend for " hotspot-name " with file = " filename " in repository = " repo-path)
          ; I'd rather not blame a third-party like Git, but it does look as if it has concurrency issues.
          ; So let's try this a few more times and may thes ynchronization primitives be with us:
          (try
            (trend-fn)
            (catch Exception _e1
              (try
                (log/info "Failed second attempt at calculating trend for " hotspot-name)
                (trend-fn)
                (catch Exception e2 ; NOTE: we just log and continue - the user probably wants the rest of the analysis results
                  (log/warn "Failed to calculate a complexity trend for " hotspot-name " resolved to " real-file-name " in the repository = " repo-path))))))))))

(defn- count-files-under-analysis
  [file-summary-file]
  (->> file-summary-file
       shared/read-csv-sans-header-from
       (remove (fn [[language _]] (= language "Ignored")))
       (map (fn [[_language file-count]] (Integer/parseInt file-count)))
       (reduce +)))

(defn- generate-all-time-hotspots
  [{:keys [evo-log code-maat-params]} destination]
    (log/trace "Running the analysis of the all time Hotspots")
    (maat/run-closed-maat evo-log "revisions" destination code-maat-params))

(def ^:private cutoff-point-for-fast-system-level-trend 500) ; files

(defn- disabled-system-trends-spec
  [{:keys [analysis-path]} n-complexity-trends]
  (let [hotspots-log (io/file analysis-path paths/hotspots-csv)]
    {:trend-filter (partial take n-complexity-trends)
     :system-trend? false
     :hotspot-input hotspots-log}))

(defn- enabled-system-trends-spec
  [{:keys [analysis-path analysis-path-fn] :as context} {:keys [include-deleted-content] :as _project}]
  (let [all-time-log (io/file analysis-path paths/all-time-revisions-csv)
        hotspots-log (io/file analysis-path paths/hotspots-csv)
        destination (analysis-path-fn paths/all-time-revisions-csv)]
    (if include-deleted-content
      (generate-all-time-hotspots context destination)
      (io/copy hotspots-log all-time-log)) ; to provide a single file for aggregated trends
    {:trend-filter identity
     :system-trend? true
     :hotspot-input all-time-log}))

(defn- make-file-filter-for-trends
  [{:keys [analysis-path] :as context} {:keys [system-level-trends] :as project} n-complexity-trends]
  (case system-level-trends

    :no-system-level-trend
    (do
      (timbre/debug "System Level Trends disabled.")
      (disabled-system-trends-spec context n-complexity-trends))

    :run-system-level-trend
    (do
      (timbre/debug "System Level Trends enabled - will run.")
      (enabled-system-trends-spec context project))

    :run-system-level-trend-when-suitable
    (let [n-files (count-files-under-analysis (io/file analysis-path paths/file-summary-csv))
          suitable? (< n-files cutoff-point-for-fast-system-level-trend)]
      (timbre/debug (str "System Level Trends are delegated to CodeScene. Got " n-files " active files, will run: " suitable?))
      (if suitable?
        (enabled-system-trends-spec context project)
        (disabled-system-trends-spec context n-complexity-trends)))))

(defn- deletion-aware-complexity-trend-calculation
  [commit-exclusion-lookup git-client content-deletion-lookup repo-scope repo-path analysis-start-date real-file-name out-filename]
  (complexity/calculate-trend commit-exclusion-lookup git-client (partial content-deletion-lookup repo-scope real-file-name) repo-path analysis-start-date real-file-name out-filename))

(defn generate-hotspot-trends-from
  [{:keys [analysis-start-date analysis-path analysis-version scrambled-translation git-client deletion-log] :as context}
   {:keys [repo-paths exclude-commits] :as project}
   n-complexity-trends]
  (let [{:keys [system-trend?] :as trend-context} (make-file-filter-for-trends context project n-complexity-trends)
        complexity-trends (complexity-trend-paths-for-hotspots trend-context analysis-version analysis-path)
        content-deletion-lookup (deleted-content/make-deletion-date-lookup-by-file-in-repo deletion-log)
        scope->repo-lookup (file-patterns/make-scope->repo-lookup repo-paths)
        commit-exclusion-lookup (exclude-commits-by-repo-lookup git-client repo-paths exclude-commits)
        trend-calculator (make-trend-calculator scope->repo-lookup
                                                analysis-start-date
                                                scrambled-translation
                                                (partial deletion-aware-complexity-trend-calculation commit-exclusion-lookup git-client content-deletion-lookup))]
    (log/info "Calculating trends in parallel.")
    (doall (pmap trend-calculator complexity-trends))
    system-trend?))

;;
;; Early warnings for complexity trends
;;

(defn as-complexity-thresholds-configuration
  [warning-thresholds {:keys [complexitytrendwarninglookbackinmonths]}]
  (-> warning-thresholds
      (dissoc :n-complexity-trends)
      (assoc :trend-warning-look-back-in-months complexitytrendwarninglookbackinmonths)))

(defn- hotstpos-with-complexity-warnigns-in
  [hs]
  (->> hs
       (map (juxt :hotspot-name :complexity-trend-classification))
       (remove (fn [[_name c]] (every? (partial = 0) c)))
       (map first)))

(defn augment-hotspots-with-trend-warnings
  [project
   {:keys [analysis-version analysis-path-fn analysis-path time-now]}
   {:keys [n-complexity-trends] :as complexity-warning-thresholds}]
  (let [hotspots-log (analysis-path-fn paths/hotspots-csv)
        hotspots-with-trend-files (complexity-trend-paths-for-hotspots {:trend-filter (partial take n-complexity-trends)
                                                                        :hotspot-input hotspots-log}
                                                                       analysis-version
                                                                       analysis-path)
        hotspots-with-trend-data-source (map (fn [{:keys [filename] :as s}]
                                               (-> s
                                                   (dissoc :filename)
                                                   (assoc :complexity-trend-dataset-fn (partial complexity-trend-step-detection/complexity-dataset-from-file filename))))
                                             hotspots-with-trend-files)
        augmented-hotspots (analysis-path-fn paths/augmented-hotspots-csv)
        hotspots-with-warning-classifications (complexity-trend-step-detection/detect-increasing-steps-in
                                                hotspots-with-trend-data-source
                                                (as-complexity-thresholds-configuration complexity-warning-thresholds project)
                                                time-now)]
    (timbre/trace "Hotspot complexity trends detected for: " (pr-str (hotstpos-with-complexity-warnigns-in hotspots-with-warning-classifications)))
    (hotspots/augment-hotspots-with-complexity-trend-warnings hotspots-log
                                                              hotspots-with-warning-classifications
                                                              augmented-hotspots)))
