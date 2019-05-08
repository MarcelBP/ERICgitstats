;;; Copyright (C) 2013-2016 Adam Tornhill
;;;

(ns evolutionary-metrics.app.app
  (:require [evolutionary-metrics.parsers.git-multi-step-parser :as git-parser]
            [evolutionary-metrics.parsers.identity :as id]
            [evolutionary-metrics.analysis.identity-analysis :as identity-analysis]
            [clojure.string :as string]
            [evolutionary-metrics.output.csv :as csv-output]
            [evolutionary-metrics.analysis.authors :as authors]
            [evolutionary-metrics.analysis.entities :as entities]
            [evolutionary-metrics.analysis.logical-coupling :as coupling]
            [evolutionary-metrics.analysis.sum-of-coupling :as soc]
            [evolutionary-metrics.analysis.summary :as summary]
            [evolutionary-metrics.analysis.churn :as churn]
            [evolutionary-metrics.analysis.effort :as effort]
            [evolutionary-metrics.app.architectural-mapper :as architectural-mapper]
            [evolutionary-metrics.app.time-based-grouper :as time-grouper]
            [evolutionary-metrics.app.ticket-grouper :as ticket-grouper]
            [evolutionary-metrics.analysis.communication :as communication]
            [evolutionary-metrics.app.content-filter :as filter-input]
            [evolutionary-metrics.app.distinct-commits :as distinct-commits]
            [evolutionary-metrics.app.author-filter :as author-filter]
            [evolutionary-metrics.app.date-filter :as date-filter]
            [evolutionary-metrics.app.team-mapper :as team-mapper]
            [evolutionary-metrics.analysis.commit-messages :as commits]
            [evolutionary-metrics.analysis.code-age :as age]
            [evolutionary-metrics.analysis.revision-churn :as revision-churn]
            [evolutionary-metrics.analysis.author-churn :as author-churn]
            [evolutionary-metrics.analysis.costs :as costs]
            [evolutionary-metrics.analysis.ticket-id :as ticket-id-analysis]
            [evolutionary-metrics.app.exclude-commits :as exclude-commits]
            [evolutionary-metrics.app.periodical-trends :as periodical-trends]
            [evolutionary-metrics.app.developer-aliases :as developer-aliases]
            [evolutionary-metrics.analysis.author-timeline :as author-timeline]
            [evolutionary-metrics.app.pair-program-knowledge-mapper :as pair-program-knowledge-mapper]))

;;; Principles:
;;;
;;; All individual parts (parsers, analyses, outputs) are kept in
;;; separate, independet units.
;;;
;;; This top-level program (app - lousy name) glues the individual
;;; parts together into a pipeline of behaviour. The parts are
;;; selected based on the option passed in from the user interface.
;;;
;;; The overall flow is:
;;;  1 Input: raw text-files (log, optional layer spec, etc)
;;;  2 Parsers: receive the Input, returns a seq of maps. Each map
;;;    describes one modification.
;;;  3 The output from the parsers is fed into the layer mapping.
;;;    This is an optional step where individual changes may be
;;;    aggregated to fit analyses at architectural boundaries.
;;;  4 The seq of maps is now transformed into Incanter datasets.
;;;  5 The analyses receive the datasets. An analysis always returns
;;;    a dataset itself.
;;;  6 The output stage receives the dataset.


;;; TODO: consider making this dynamic in order to support new
;;;       analysis methods as plug-ins.
(def ^:const supported-analysis
  {"all-authors"           authors/all-authors
   "revisions"             entities/by-revision
   "coupling"              coupling/by-degree
   "soc"                   soc/by-degree
   "summary"               summary/overview
   "identity"              identity-analysis/identity-dataset
   "abs-churn"             churn/absolutes-trend
   "abs-churn-by-ticket-id" ticket-id-analysis/absolute-by-ticket-id
   "author-churn"          author-churn/by-author
   "entity-churn"          churn/by-entity
   "entity-ownership"      churn/as-ownership
   "main-dev"              churn/by-main-developer
   "refactoring-main-dev"  churn/by-refactoring-main-developer
   "entity-effort"         effort/as-revisions-per-author
   "main-dev-by-revs"      effort/as-main-developer-by-revisions
   "fragmentation"         effort/as-entity-fragmentation
   "communication"         communication/by-shared-entities
   "messages"              commits/by-word-frequency
   "modus-commit-trend"    commits/trends
   "age"                   age/by-age
   "revision-churn"        revision-churn/revisions-by-date
   "diffusion-by-revision" revision-churn/diffusion-by-revision
   "costs"                 costs/costs-of
   "author-timeline"       author-timeline/active-authors-timeline})

(def ^:const analysis-names (->> (keys supported-analysis) sort (string/join ", ")))

(defn- fail-for-invalid-analysis
  [requested-analysis]
  (throw (IllegalArgumentException.
          (str "Invalid analysis requested: " requested-analysis ". "
               "Valid options are: " analysis-names))))

(defn- make-analysis
  "Returns the analysis to run while closing over the options.
   Each returned analysis method takes a single data set as argument."
  [options]
  (if-let [analysis (supported-analysis (options :analysis))]
    #(analysis % options)
    (fail-for-invalid-analysis (options :analysis))))

(defn- run-parser-in-error-handling-context
  [parse-fn vcs-name]
  (try
    (parse-fn)
    (catch IllegalArgumentException ae
      (throw (IllegalArgumentException. "Failed to run the analysis." ae)))
    (catch Exception e
      (throw (IllegalArgumentException.
              (str vcs-name ": Failed to parse the given file - is it a valid logfile?" e))))))

(defn- git->modifications
  [logfile-name options]
  (run-parser-in-error-handling-context
    (partial git-parser/parse-from-file logfile-name options)
    "git"))

(defn- identity->modifications
  [logfile-name options]
  (run-parser-in-error-handling-context
   #(id/parse-log logfile-name options)
   "identity"))

(defn- parser-from
  [{:keys [version-control]}]
  (case version-control
    "git" git->modifications
    "id"  identity->modifications
    (throw (IllegalArgumentException.
            (str "Invalid --version-control specified: " version-control
                 ". Supported options are: git, or id.")))))

(defn- make-stdout-output [options]
  (if-let [n-out-rows (:rows options)]
    #(csv-output/write-to :stream % n-out-rows)
    #(csv-output/write-to :stream %)))

(defn- make-output [options]
  (if (:identity-output options)
    identity
    (if-let [output-file (:outfile options)]
      #(csv-output/write-to-file output-file :stream %)
      (make-stdout-output options))))

(defn- throw-internal-error [e]
  (throw (IllegalArgumentException.
          (str "Internal error - please report it. Details = "
               (.getMessage e)) e)))

(defn- run-with-recovery-point
  [analysis-fn changes output-fn!]
  (try
    (output-fn! (analysis-fn changes))
    (catch AssertionError e ; typically a pre- or post-condition
      (throw-internal-error e))
    (catch Exception e
      (throw-internal-error e))))

(defn- parse-commits-to-dataset
  [vcs-parser logfile-name options]
  (->>
    (vcs-parser logfile-name options)
    (distinct-commits/distinct-commits options)
    (exclude-commits/by-commit-hash options)
    (pair-program-knowledge-mapper/resolve-contributors options) ; Might generate new aliases that we want to resolve...
    (developer-aliases/resolve-authors options) ; ..in this step. From now on all author aliases are resolved and we can start to map teams, etc.
    (architectural-mapper/aggregate-on-boundaries options)
    (time-grouper/by-same-author-on-same-day options)
    (ticket-grouper/by-ticket-id options)
    (date-filter/by-date-range options)
    (periodical-trends/by-weekly-commits options)
    (author-filter/by-excluded-author-names options)
    (author-filter/by-author-names options)
    (team-mapper/by-teams options)
    (filter-input/by-glob-patterns options)
    (filter-input/by-input-file options)))

(defn run
  "Runs the application using the given options.
   The options are a map with the following elements:
    :module - the VCS to parse
    :analysis - the type of analysis to run
    :rows - the max number of results to include"
  [logfile-name options]
  (let [vcs-parser (parser-from options)
        commits (parse-commits-to-dataset vcs-parser logfile-name options)
        analysis (make-analysis options)
        output! (make-output options)]
      (run-with-recovery-point analysis commits output!)))
