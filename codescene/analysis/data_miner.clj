(ns codescene.analysis.data-miner
  (:require [clj-time.core :as t]
            [clj-time.format :as tf]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [codescene.analysis.paths :as paths]
            [codescene.analysis.project-management-metrics :as pm]
            [codescene.analysis.pair-programming-configurer :as pp-conf]
            [codescene.analysis.specs :as specs]
            [codescene.mining.evolution :as mining]
            [evolutionary-metrics.app.app :as maat]
            [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [evolutionary-metrics.mining.scrambler :as scrambler]
            [evolutionary-metrics.mining.vcs :as vcs]
            [evolutionary-metrics.complexity.architectural-loco :as architectural-loco]
            [evolutionary-metrics.complexity.loco :as loco]
            [taoensso.timbre :as log]
            [clj-time.core :as tc]
            [evolutionary-metrics.parsers.git-multi-step-parser :as git-parser]
            [codescene.note-watch.renames :as renames]))

(defn- miner-based-on
  [complete-history-for-social]
  (if complete-history-for-social
    (fn [git-client repo-path _git-start-date rename-limit-depth] (vcs/mine-complete-git-log-with-rename-detection git-client repo-path rename-limit-depth))
    vcs/mine-git-with-rename-detection))

(def ^:private complexity-log-name "complexity.csv")

(defn commit-filter-from
  "The user may specify specific commits to exclude. This is
   typically used to reduce bias in the social analyses in case
   a single developer imported an existing codebase with an
   initial single commit."
  [{:keys [exclude-commits] :as _project} git-client repo-path]
  (if (seq exclude-commits)
    (let [short-commits (map (partial vcs/to-short-hash git-client repo-path) exclude-commits)]
      (log/info "Excluding commits: " (pr-str short-commits))
      {:exclude-commits short-commits})
    {}))

(defn- parsed-evo-data-for
  [raw-log git-client repo-path
   {:keys [exclusionfilter modus-commit-message-pattern ticketidpattern
           pair-programming-pattern handle-nonascii-paths?
           hotspot-defect-commit-pattern] :as project}
   repo-scope parsed-log-destination intermediate-log-destination]
  (let [optional-filters (mining/optional-git-mining-arguments-for project repo-scope repo-path git-client)
        commit-filter (commit-filter-from project git-client repo-path)
        mandatory-parse-options {:exclusionfilter exclusionfilter
                                 :handle-nonascii-paths? handle-nonascii-paths?
                                 ; Optimization: pass along the commit message extraction patterns to the parser
                                 :expression-to-match modus-commit-message-pattern
                                 :message-extraction-pattern ticketidpattern
                                 :pair-programming-pattern pair-programming-pattern
                                 :hotspot-defect-commit-pattern hotspot-defect-commit-pattern}
        options (merge mandatory-parse-options optional-filters commit-filter)]
    (log/trace "Parse evolutionary data using the options: " (pr-str options))
    (let [parse-results (git-parser/parse-from-file raw-log options
                                                    (renames/make-write-intermediate-results-for-renaming-fn intermediate-log-destination))]
      (log/debug "Persisting parsed evolutionary data to results: " parsed-log-destination)
      (with-open [out-file (io/writer parsed-log-destination)]
        (csv/write-csv out-file [["author" "rev" "date" "entity" "message" "loc-added" "loc-deleted" "author-email" "basic-date-time"]])
        (csv/write-csv out-file (map (juxt :author :rev :date :entity :message :loc-added :loc-deleted :author-email :basic-date-time) parse-results))))))

(defn- ->git-start-date-time
  "To ensure that the current time does not affect the start date, we create
   a new DateTime with time 00:00 in the local timezone."
  [date]
  {:pre [(s/valid? ::specs/analysis-start-date date)]}
  (let [dwt (tf/parse (tf/with-zone (:year-month-day tf/formatters) (t/default-time-zone)) date)
        dwt-string (tf/unparse (:date-time tf/formatters) dwt)]
    {:git-start-date-time-object dwt
     :git-start-date-as-string dwt-string}))

(defn- mine-deleted-content-from-single-repo
  [project git-client git-start-date output-dir [index repo-path]]
  (log/trace "Mining deleted content for <" repo-path "> starting with " git-start-date)
  (let [raw-log (paths/as-indexed-file-name output-dir "partialgitdeletionhistory" index "log")
        parsed-log (paths/as-indexed-file-name output-dir "partialdeletion" index "id")
        intermediate-log-destination (paths/as-indexed-file-name output-dir "intermediate-results-for-renaming" index "csv")
        repo-scope (file-patterns/final-segment repo-path)
        mined-data (vcs/mine-deleted-content git-client repo-path git-start-date)]
    (spit raw-log mined-data)
    (parsed-evo-data-for raw-log git-client repo-path project repo-scope parsed-log intermediate-log-destination)))

(defn- mine-deleted-content?
  [{:keys [system-level-trends include-deleted-content]}]
  (and (not (= system-level-trends :no-system-level-trend))
       include-deleted-content))

(defn- mine-deleted-content
  "In case system level trends are used we want to be able to account for deleted content, which is
   generally an improvement.
   Note that we only mine this data if it's needed since it adds a lot of time to the overall analysis."
  [output-dir {:keys [repo-paths mine-git-in-parallel] :as project} git-start-date git-client git-log-name]
  (let [combined-deletion-log (paths/as-child-path output-dir git-log-name)
        indexed-paths (map-indexed vector repo-paths)]
    (if (mine-deleted-content? project)
      (do
        (if mine-git-in-parallel
          (doall
            (pmap (partial mine-deleted-content-from-single-repo project git-client git-start-date output-dir) indexed-paths))
          (doseq [a-repo indexed-paths]
            (mine-deleted-content-from-single-repo project git-client git-start-date output-dir a-repo)))
        (paths/concat-csv-file-content output-dir "partialdeletion" combined-deletion-log))
      (spit combined-deletion-log "")) ; an empty log means no deletions, which helps us avoid special cases later.
    combined-deletion-log))

(defn- mine-single-git-repo
  [{:keys [git-rename-limit] :as project} mine-fn git-client git-start-date output-dir [index repo-path]]
  (log/trace "Mining evo data for <" repo-path "> starting with " git-start-date)
  (let [raw-log (paths/as-indexed-file-name output-dir "partialgithistory" index "log")
        parsed-log (paths/as-indexed-file-name output-dir "partialidentity" index "id")
        intermediate-log-destination (paths/as-indexed-file-name output-dir "intermediate-results-for-renaming" index "csv")
        repo-scope (file-patterns/final-segment repo-path)
        rename-limit-depth (or git-rename-limit 1000)
        mined-data (mine-fn git-client repo-path git-start-date rename-limit-depth)]
    (spit raw-log mined-data)
    (parsed-evo-data-for raw-log git-client repo-path project repo-scope parsed-log intermediate-log-destination)))

(defn- mine-evo-data
  "Mines the evolutionary data in a two step process:
    1. Get the raw data out of Git.
    2. Parse it into our internal format.
  The mining is done per repository before we combine the partial Git logs into
  a combined log which we then use for our analyses."
  [output-dir {:keys [repo-paths complete-history-for-social mine-git-in-parallel] :as project} git-start-date git-log-name git-client]
  (let [combined-evo-log (paths/as-child-path output-dir git-log-name)
        mine-fn (miner-based-on complete-history-for-social)
        indexed-paths (map-indexed vector repo-paths)]
    (if mine-git-in-parallel
      (doall
        (pmap (partial mine-single-git-repo project mine-fn git-client git-start-date output-dir) indexed-paths))
      (doseq [a-repo indexed-paths]
        (mine-single-git-repo project mine-fn git-client git-start-date output-dir a-repo)))
    (paths/concat-csv-file-content output-dir "partialidentity" combined-evo-log)))

(defn- mine-evo-data-depending-on-license
  [output-dir project git-start-date git-log-name scrambled-names {:keys [scramble-enabled git-client]}]
  (let [input-file-name (mine-evo-data output-dir project git-start-date git-log-name git-client)]
    (if scramble-enabled
      (let [out-file-name input-file-name]
        (scrambler/scramble-evo-data scrambled-names input-file-name out-file-name))
      input-file-name)))

(defn generate-distinct-commits-log
  "Applies input filters to the parsed log generated in an earlier step.
   The filtered log is used by the analyses that only want to operate on
   what's present in the current snapshot of the repository (hotspots, temporal, but not
   churn and trend analyses"
  [{:keys [selectdistinctcommits]} parsed-log output-file-name]
  (if (= 1 selectdistinctcommits)
    (let [all-parameters {:log                     parsed-log
                          :version-control         "id"
                          :analysis                "identity"
                          :select-distinct-commits true
                          :outfile                 output-file-name}]
      (maat/run parsed-log all-parameters)
      output-file-name)
    parsed-log))

(defn generate-filtered-evo-data
  "Applies input filters to the parsed log generated in an earlier step.
   The filtered log is used by the analyses that only want to operate on
   what's present in the current snapshot of the repository (hotspots, temporal, but not
   churn and trend analyses"
  [parsed-log optional-parameters output-file-name]
  (let [all-parameters {:log                    parsed-log
                        :version-control        "id"
                        :analysis               "identity"
                        :outfile                output-file-name}]
    (log/debug "Generating filtered evolutionary data: " output-file-name)
    (maat/run parsed-log (merge all-parameters optional-parameters))
    output-file-name))

(defn- persist-transformations
  [transformations output-file-name]
  (let [persistable (map (fn [{:keys [pattern transformation]}] [pattern transformation]) transformations)]
    (with-open [out-file (io/writer output-file-name)]
      (binding [*out* out-file]
        (csv/write-csv *out* [["pattern" "transformation"]])
        (csv/write-csv *out* persistable))))
  output-file-name)

(defn- generate-architectural-evo-log
  "Applies optional transformations to the mined evolutionary log. The resulting
   log will contain logical components rather than individual files. This is
   used to analyse the repositories on an architectural level."
  [parsed-log transformations-file-name output-file-name]
  (let [group-parameters {:log             parsed-log
                          :version-control "id"
                          :analysis        "identity"
                          :group           transformations-file-name
                          :outfile         output-file-name}]
    (maat/run parsed-log group-parameters)
    output-file-name))

(defn- concat-file-summaries
  [dir csv-base-name destination-file-name]
  (as-> csv-base-name $
        (paths/file-seq-matching dir $)
        (map #(.getAbsolutePath %) $)
        (loco/combine-multiple-summaries $ destination-file-name)))

(defn- with-report-of-ignored-content
  [ignore-stats-file-name options]
  (merge options {:ignored-files-fn (fn [ignored]
                                      (doseq [i ignored]
                                        (log/info "Ignored file reported: " i)
                                        (spit ignore-stats-file-name (str i "\n") :append true)))}))

(defn- with-files-excluded-by-extension
  [options exclusionfilter]
  (merge options {:exclude-files (mining/extensions->glob-patterns exclusionfilter)}))

(defn- with-files-under-vcs-only
  [{:keys [filter-repository-content-on-git-ls]} options files-in-vcs-fn]
  (if filter-repository-content-on-git-ls
    (let [[res content] (files-in-vcs-fn)]
      (if res
        (merge options {:keep-specific-file-fn (into #{} content)})
        (do
          (log/error "Failed to read files under version-control. Error: " content)
          (throw (Exception. "Failed to access Git. See detailed log for more information.")))))
    (merge options {:keep-specific-file-fn (constantly true)})))

(defn mine-complexity
  [{:keys [repo-paths exclusionfilter auto-detect-text-files handle-nonascii-paths?] :as project} {:keys [git-client]} output-dir]
  (let [combined-log (paths/as-child-path output-dir complexity-log-name)
        combined-summaries-log (paths/as-child-path output-dir paths/file-summary-csv)
        indexed-paths (map-indexed vector repo-paths)
        ignored-files-stats (paths/as-child-path output-dir "ignoredfiles.txt")]
    (doseq [[index repo-path] indexed-paths]
      (let [file-summary-log (paths/as-indexed-file-name output-dir "filesummary" index "csv")
            partial-log (paths/as-indexed-file-name output-dir "complexity" index "csv")
            repo-scope (file-patterns/final-segment repo-path)
            files-in-git-fn (partial vcs/repository-content git-client handle-nonascii-paths? repo-path) ; filter away files that aren't under version control
            options-1 (with-files-under-vcs-only
                      project
                      (with-report-of-ignored-content ignored-files-stats
                                                      (mining/as-exclusion-options project repo-scope))
                      files-in-git-fn)
            options (merge options-1 {:auto-detect-text-files auto-detect-text-files})
            detailed-options (with-files-excluded-by-extension options exclusionfilter)]
        (log/trace "Mining complexity for <" repo-path "> with options: " (pr-str (dissoc options :keep-specific-file-fn)))
        (loco/summary repo-path options file-summary-log)
        (loco/detailed-stats repo-path detailed-options partial-log)))
    (concat-file-summaries output-dir "filesummary" combined-summaries-log)
    (paths/concat-csv-file-content output-dir "complexity" combined-log)))

(defn mine-complexity-depending-on-license
  [project output-dir {:keys [scramble-enabled] :as analysis-params}]
  (let [file-name (mine-complexity project analysis-params output-dir)]
    (if scramble-enabled
      (let [out-file-name file-name
            scrambled-translations (loco/scramble-all-file-names file-name out-file-name)]
        [out-file-name scrambled-translations])
      [file-name {}])))

;; Scramble end

(defn- mine-architectural-complexity
  [complexity-by-file transformations-log-name output-file-name]
  (architectural-loco/detailed-stats-by-components complexity-by-file transformations-log-name output-file-name)
  output-file-name)

(defn- as-mining-parameter
  [authors-to-exclude]
  (if (empty? authors-to-exclude)
    {}
    {:excluded-authors authors-to-exclude}))

(defn- trend-cutoff-date-from
  "Calculate a date that lies n days in the past. Don't care if it is before the
   start of the analysis - that only means we don't get any data later, but that will
   adjust itself as time moves on and new analyses are run at later dates."
  [{:keys [spottrendspan]} time-now]
  {:pre [(s/valid? ::specs/spottrendspan spottrendspan)]}
  (->> (t/days spottrendspan)
       (t/minus time-now)))

(defn- sensible-end-date-from
  "Some projects have commits that a far in the future. These commits
  destroy the trend analyses by introducing noise."
  [time-now]
  (tc/plus time-now (tc/weeks 2)))

(defn- write-csv-to
  [file-name headers rows]
  (with-open [out-file (io/writer file-name)]
    (csv/write-csv out-file headers)
    (csv/write-csv out-file rows)))

(defn- store-pm-metrics-in-files
  [analysis-path-fn cost-unit {:keys [items]} {:keys [supported-types items-with-types]}]
  (let [data-description-file-name (analysis-path-fn paths/pm-data-description-csv)
        cost-file-name (analysis-path-fn paths/pm-item-input-data-csv)
        type-of-work-file-name (analysis-path-fn "typeofwork.csv")]
    (log/trace "PM metrics use the cost unit: " cost-unit)
    (write-csv-to data-description-file-name [["field" "value"]] [["costunit" cost-unit]])
    (log/trace "Retrieved " (count items) " cost items")
    (write-csv-to cost-file-name [["id" "cost"]] items)
    (log/trace "Retrieved the following type of work for cost calculations: " (string/join "," supported-types))
    (write-csv-to type-of-work-file-name [(into ["id"] supported-types)] items-with-types)
    {:pm-costs-log        cost-file-name
     :pm-type-of-work-log type-of-work-file-name}))

(defn- mine-project-metrics
  "Add project management metrics like costs in case we have an integration to
   a third-party-tool."
  [analysis-path-fn project]
  (if (pm/use-project-management-metrics? project)
    (let [projects (pm/get-projects project)
          cost-unit (->> projects (map (comp :type :costUnit)) first)
          costs (pm/cost-by-identifiable-item projects)
          type-of-work (pm/type-of-work-by-identifiable-item projects)]
      (store-pm-metrics-in-files analysis-path-fn cost-unit costs type-of-work))
    {}))

(defn- logs-per-analysis-category
  "At this stage we have mined all data we need from Git and the file system.
   Our next step is to clean this data and generate distinct logs tailored to
   the type of analysis (social, teams, architectural, etc).
   The main reason we introduce a separate function for this step is because we
   want to be able to debug failed analyses (typically due to performance issues) by
   just specifying a folder with existing raw logs."
  [analysis-path-fn
   {:keys [team-analysis-start-date developer-alias-map] :as project}
   {:keys [authors-to-exclude architectural-transformations] :as _analysis-params}
   time-now
   {:keys [mined-complexity complete-evo-log deletion-log mined-git-start-date] :as _raw_logs}]
  (let [[complexity-log scrambled-names] mined-complexity
        {:keys [git-start-date-time-object]} mined-git-start-date
        distinct-commits-log-name (analysis-path-fn "completeparsedevolution.csv")
        distinct-commits-log (generate-distinct-commits-log project complete-evo-log distinct-commits-log-name)
        evo-log-name (analysis-path-fn "parsedevolution.csv")
        evo-log (generate-filtered-evo-data distinct-commits-log
                                            {:filter-start-date git-start-date-time-object
                                             :filter-end-date (sensible-end-date-from time-now)
                                             :developer-alias-map developer-alias-map}
                                            evo-log-name)
        evo-log-resolved-pairs-name (analysis-path-fn "parsedevoresolved.csv")
        evo-log-resolved-pairs (generate-filtered-evo-data evo-log
                                                           (pp-conf/enable-pair-programming-when-specified
                                                             project {:developer-alias-map developer-alias-map})
                                                           evo-log-resolved-pairs-name)
        technical-analysis-log-name (analysis-path-fn paths/technical-vcs-log)
        technical-log (generate-filtered-evo-data evo-log
                                                  {:filter-specified-files complexity-log}
                                                  technical-analysis-log-name)
        individual-social-log-name (analysis-path-fn "individualsociallog.id")
        individual-social-log-options (pp-conf/enable-pair-programming-when-specified project
                                                                                      (merge {:filter-specified-files complexity-log
                                                                                              :developer-alias-map developer-alias-map}
                                                                                             (as-mining-parameter authors-to-exclude)))
        individual-social-log (generate-filtered-evo-data distinct-commits-log ; NOTE: all the way back to the beginning of the repo!
                                                          individual-social-log-options
                                                          individual-social-log-name)
        social-analysis-log-name (analysis-path-fn "filteredsocial.id")
        social-log (generate-filtered-evo-data technical-log
                                               (pp-conf/enable-pair-programming-when-specified
                                                 project
                                                 (merge {:developer-alias-map developer-alias-map} (as-mining-parameter authors-to-exclude)))
                                               social-analysis-log-name)
        safe-team-analysis-start-date (->git-start-date-time team-analysis-start-date)
        team-analysis-log-name (analysis-path-fn "teamfilteredsocial.id")
        team-analysis-log (generate-filtered-evo-data social-log
                                                      {:filter-start-date (:git-start-date-time-object safe-team-analysis-start-date)}
                                                      team-analysis-log-name)
        trend-log-name (analysis-path-fn "historiclog.id")
        trend-log (generate-filtered-evo-data technical-log {:filter-end-date (trend-cutoff-date-from project time-now)} trend-log-name)
        transformations-log-name (analysis-path-fn "transformations.csv")
        persisted-transformations (persist-transformations architectural-transformations transformations-log-name)
        architectural-log-name (analysis-path-fn "architectural.id")
        architectural-log (generate-architectural-evo-log technical-log persisted-transformations architectural-log-name)
        architectural-knowledge-log-name (analysis-path-fn "architecturalknowledge.id")
        architectural-knowledge-log (generate-architectural-evo-log individual-social-log ; NOTE: all the way back to the beginning of the repo!
                                                                    persisted-transformations
                                                                    architectural-knowledge-log-name)
        architectural-team-knowledge-log-name (analysis-path-fn "architecturalteamknowledge.id")
        architectural-team-knowledge-log (generate-architectural-evo-log team-analysis-log
                                                                         persisted-transformations
                                                                         architectural-team-knowledge-log-name)
        architectural-complexity-log-name (analysis-path-fn "archcomplexity.csv")
        architectural-complexity-log (mine-architectural-complexity complexity-log transformations-log-name architectural-complexity-log-name)]
    {:scrambled-translation        (clojure.set/map-invert scrambled-names)
     :evo-log                      evo-log
     :evo-log-resolved-pairs       evo-log-resolved-pairs
     :filtered-log                 technical-log
     :individual-socal-log         individual-social-log
     :social-log                   social-log
     :team-analysis-log            team-analysis-log
     :trend-log                    trend-log
     :complexity-log               complexity-log
     :architectural-log            architectural-log
     :architectural-knowledge-log  architectural-knowledge-log
     :architectural-team-knowledge-log architectural-team-knowledge-log
     :architectural-complexity-log architectural-complexity-log
     :deletion-log                 deletion-log}))

(defn- mine-analysis-logs
  [analysis-path analysis-path-fn
   {:keys [analysis-start-date] :as project}
   {:keys [git-client] :as analysis-params}
   time-now]
  (let [[complexity-log scrambled-names] (mine-complexity-depending-on-license project analysis-path analysis-params)
        {:keys [git-start-date-as-string] :as mined-git-start-date} (->git-start-date-time analysis-start-date)
        complete-evo-log (mine-evo-data-depending-on-license analysis-path project git-start-date-as-string "completeevolution.id" scrambled-names analysis-params)
        deletion-log (mine-deleted-content analysis-path project git-start-date-as-string git-client "completedeletion.id")]
    (logs-per-analysis-category analysis-path-fn
                                project
                                analysis-params
                                time-now
                                {:mined-complexity [complexity-log scrambled-names]
                                 :complete-evo-log complete-evo-log
                                 :deletion-log deletion-log
                                 :mined-git-start-date mined-git-start-date})))

(s/fdef mine-logs
        :args (s/cat :analysis-path ::specs/analysis-path
                     :analysis-path-fn ::specs/analysis-path-fn
                     :project ::specs/project
                     :analysis-params ::specs/analysis-context-params
                     :time-now ::specs/time-now)
        :ret ::specs/mine-logs-result)

(defn mine-logs
  [analysis-path analysis-path-fn
   project
   analysis-params
   time-now]
  (let [analysis-logs (mine-analysis-logs analysis-path analysis-path-fn project analysis-params time-now)
        pm-logs (mine-project-metrics analysis-path-fn project)]
    (merge analysis-logs pm-logs)))

(defn generate-logs-from-existing-raw-data
  "This is debug functionality that we use when we already have Git logs and complexity logs, but
   no access to the real repository.
   We simply start in the middle of the chain by adapting to the required input and generate the
   log files we need.
   Use this function to profile and time the mining stage.
   Note that it (currently) only supports single repo projects for simplicity."
  [analysis-directory
   {:keys [exclusion-filter mined-git-start-date time-now repo-scope pair-programming-pattern team-analysis-start-date]}]
  (let [analysis-path-fn (partial paths/as-child-path analysis-directory)
        raw-log (analysis-path-fn "partialgithistory0.log")
        parsed-log (analysis-path-fn "partialidentity0.id")
        intermediate-log-destination (analysis-path-fn "intermediate-results-for-renaming0.csv")
        repo-path "faked/path"
        project {:exclusionfilter exclusion-filter
                 :modus-commit-message-pattern ""
                 :ticketidpattern ""
                 :spottrendspan 30
                 :pair-programming-pattern pair-programming-pattern
                 :team-analysis-start-date team-analysis-start-date}]
    (parsed-evo-data-for raw-log "git" repo-path project repo-scope parsed-log intermediate-log-destination)
    (let [complete-evo-log-name (analysis-path-fn "completeevolution.id")
          complete-evo-log (paths/concat-csv-file-content analysis-directory "partialidentity" complete-evo-log-name)
          deletion-log "fake it since we will not use it"
          complexity-log (analysis-path-fn "complexity.csv")
          analysis-params {:authors-to-exclude ""
                           :architectural-transformations ""}]
      (logs-per-analysis-category analysis-path-fn
                                  project
                                  analysis-params
                                  time-now
                                  {:mined-complexity [complexity-log #{}]
                                   :complete-evo-log complete-evo-log
                                   :deletion-log deletion-log
                                   :mined-git-start-date mined-git-start-date}))))
