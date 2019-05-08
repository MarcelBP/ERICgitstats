(ns codescene.analysis.analysis-task-impl
  (:require [codescene.analysis.closed-maat-proxy :as maat]
            [codescene.analysis.data-miner :as data-miner]
            [codescene.analysis.paths :as paths]
            [codescene.analysis.specs :as specs]
            [codescene.mining.evolution :as evolution]
            [clojure.java.io :as io]
            [semantic-csv.core :as sc]
            [evolutionary-metrics.mergers.communication-edge-bundle :as communication]
            [evolutionary-metrics.mergers.edge-bundle :as edge]
            [evolutionary-metrics.trends.age-trend :as age-trend]
            [evolutionary-metrics.trends.code-growth :as code-growth]
            [evolutionary-metrics.trends.rolling-churn :as churn]
            [evo-pattern-detector.spots :as evo-patterns]
            [taoensso.timbre :as log]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.trends.dates :as dates]
            [codescene.biomarkers.code-markers-analysis :as cma]
            [codescene.note-watch.utils :as nu]))

(def generate-temporal-edge-bundle edge/generate-bundle)

(def generate-communication-edge-bundle communication/generate-bundle)

(defn- filter-hotspots
  [{:keys [repo-paths]} hotspots-file notes-risks]
  (let [hotspots (->> hotspots-file
                      sc/slurp-csv
                      (sc/cast-with {:revisions sc/->int :code sc/->int}))
        hotspots-target (->> hotspots
                             (map :module)
                             (map (partial cma/with-resolved-file-name repo-paths)))]
    (->> hotspots
         (map vector hotspots-target)
         (filter (comp (partial nu/non-resolved-hotspot? notes-risks) first))
         (map second)
         (map (juxt :module :revisions :code :warning)))))

(defn classify-hotspots
  [project {:keys [analysis-path-fn evo-log hotspot-prioritization-fn]} notes-risks]
   ; parameterize to let the tests override the default non-deterministic algorithm
   (let [options (if (some? hotspot-prioritization-fn)
                   {:clustering-fn hotspot-prioritization-fn}
                   {})
         hotspots (filter-hotspots project
                                   (analysis-path-fn paths/augmented-hotspots-csv)
                                   notes-risks)]
     (evo-patterns/classify-hotspots
       evo-log
       hotspots
       (analysis-path-fn paths/fragmentation-csv-file-name)
       (analysis-path-fn paths/temporal-soc-csv)
       (analysis-path-fn paths/classified-spots-csv)
       (analysis-path-fn paths/classified-spot-stats-csv)
       options)))

(def generate-age-trend age-trend/code-age-frequency)

(defn generate-churn-trend
  [{:keys [rollingaveragedays]} churn-log destination]
  (churn/calculate-churn-trend churn-log rollingaveragedays destination))

(defn generate-loc-trend
  [abs-churn file-summary destination]
  (code-growth/calculate-code-growth abs-churn file-summary destination))

(defn age-reference-time
  [{:keys [age-is-last-commit] :as _project} {:keys [filtered-log] :as _logs} time-now]
  (if age-is-last-commit
    (let [last-commit-date (->> filtered-log sc/slurp-csv (map :date) (sort (comp - compare)) first)]
      (or last-commit-date (dates/date->string time-now)))
    (dates/date->string time-now)))

(s/fdef make-analysis-context!
        :args (s/cat :analysis-path ::specs/filename
                     :project ::specs/project
                     :warning-reporter-fn ::specs/warning-reporter-fn
                     :analysis-params ::specs/analysis-context-params
                     :time-now ::specs/time-now
                     :analysis-version ::specs/analysis-version)
        :ret ::specs/analysis-context)

(defn make-analysis-context!
  [analysis-path
   {:keys [repo-paths analysis-start-date update-repositories update-subcommand prune-removed-remote-branches] :as project}
   warning-reporter-fn
   {:keys [git-client] :as analysis-params}
   time-now
   analysis-version]
  (log/infof "Mining data using Git client '%s'." git-client)
  (let [analysis-path-fn (partial paths/as-child-path analysis-path)]
    (io/make-parents (analysis-path-fn "start_analysis"))   ; ensure path exists
    (evolution/update-with-fallback repo-paths
                                    update-repositories
                                    (or update-subcommand :pull)
                                    (and prune-removed-remote-branches)
                                    warning-reporter-fn
                                    git-client)
    (let [logs (data-miner/mine-logs analysis-path analysis-path-fn project analysis-params time-now)
          age-reference (age-reference-time project logs time-now)
          optional-hotspot-prioritization-fn (get project :hotspot-prioritization-fn)
          hotspot-prioritization-context (if (some? optional-hotspot-prioritization-fn)
                                           {:hotspot-prioritization-fn optional-hotspot-prioritization-fn}
                                           {})]
      (merge logs
             analysis-params
             hotspot-prioritization-context
             {:analysis-path       analysis-path
              :analysis-path-fn    analysis-path-fn
              :analysis-version    analysis-version
              :analysis-start-date analysis-start-date
              :time-now time-now
              :age-time-now age-reference
              :code-maat-params    (assoc
                                     (maat/project->closed-maat-parameters project)
                                     :age-time-now age-reference)}))))

(defn analyze-on-complete-evolution
  "Runs the given analysis with the provided context.
   The analysis results will be delivered to a file on the
   analysis path. The file name is created by convention: the
   name of the analysis + .csv"
  [{:keys [evo-log analysis-path-fn code-maat-params]} analysis]
  (let [destination (analysis-path-fn (paths/analysis->result-path analysis))]
    (log/debug "Running the analysis " analysis)
    (maat/run-closed-maat evo-log analysis destination code-maat-params)))

(defn analyze-author-statistics
  "Separate as we need to take possible pair-programming into account."
  [{:keys [evo-log-resolved-pairs analysis-path-fn code-maat-params]}
   analysis]
  (let [destination (analysis-path-fn (paths/analysis->result-path analysis))]
    (log/debug "Running the author analysis " analysis)
    (maat/run-closed-maat evo-log-resolved-pairs analysis destination code-maat-params)))

(defn analyze-technical
  "Runs the given analysis with the provided context on the filtered evolutionary log file.
   The analysis results will be delivered to a file on the
   analysis path. The file name is created by convention: the
   name of the analysis + .csv"
  [{:keys [filtered-log analysis-path-fn code-maat-params]} analysis]
  (let [destination (analysis-path-fn (paths/analysis->result-path analysis))]
    (log/debug "Running the technical analysis " analysis)
    (maat/run-closed-maat filtered-log analysis destination code-maat-params)))

(defn analyze-social
  "Runs the given analysis with the provided context on an evolutionary log file that
   is filtered with respect to social relevant information (e.g. authors may be excluded).
   The analysis results will be delivered to a file on the
   analysis path. The file name is created by convention: the
   name of the analysis + .csv"
  [_project
   {:keys [individual-socal-log analysis-path-fn code-maat-params]}
   analysis]
  (let [destination (analysis-path-fn (paths/analysis->result-path analysis))]
    (log/debug "Running the social analysis " analysis)
    (maat/run-closed-maat individual-socal-log analysis destination code-maat-params)))

(defn- input-paths-exist?
  [paths]
  (->> paths
       (map io/as-file)
       (every? #(.exists %))))

(defn visualize
  "Creates a visualization content by invoking the
   specific visualization function provided in the
   description.
   A visualization depends upon previous analysis results.
   These results are given in the input vector and their
   corresponding files are retrieved using the same naming
   convention as we used when we created the analysis result."
  [{:keys [analysis-path-fn]}
   {:keys [results runner-fn input]}]
  (let [destinations (map analysis-path-fn results)
        input-paths (map (comp analysis-path-fn paths/analysis->result-path) input)
        all-args (concat input-paths destinations)]
    (if (input-paths-exist? input-paths)
      (apply runner-fn all-args)
      (log/warn "Skipping the creation of" (clojure.string/join "," results) "because some input paths don't exist."))))
