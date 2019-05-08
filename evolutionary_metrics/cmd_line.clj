;;; Copyright (C) 2013 Adam Tornhill
;;;
;;; Distributed under the GNU General Public License v3.0,
;;; see http://www.gnu.org/licenses/gpl.html

(ns evolutionary-metrics.cmd-line
  (:gen-class)
  (:require [evolutionary-metrics.app.app :as app]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]))

(def cli-options
  [["-l" "--log LOG" "Log file with input data"]
   ["-c" "--version-control VCS" "Input vcs module type: supports svn, git, tfs, hg or id"]
   ["-a" "--analysis ANALYSIS"
    (str "The analysis to run (" app/analysis-names  ")")
    :default "authors"]
   [nil "--input-encoding INPUT-ENCODING" "Specify an encoding other than UTF-8 for the log file"]
   ["-r" "--rows ROWS" "Max rows in output" :parse-fn #(Integer/parseInt %)]
   ["-o" "--outfile OUTFILE" "Write the result to the given file name"]
   ["-g" "--group GROUP" "A CSV file with a pre-defined set of layers. The data will be aggregated according to the group of layers."]
   ["-n" "--min-revs MIN-REVS" "Minimum number of revisions to include an entity in the analysis"
    :default 10 :parse-fn #(Integer/parseInt %)]
   ["-m" "--min-shared-revs MIN-SHARED-REVS" "Minimum number of shared revisions to include an entity in the analysis"
    :default 10 :parse-fn #(Integer/parseInt %)]
   ["-i" "--min-coupling MIN-COUPLING" "Minimum degree of coupling (in percentage) to consider"
    :default 30 :parse-fn #(Integer/parseInt %)]
   ["-x" "--max-coupling MAX-COUPLING" "Maximum degree of coupling (in percentage) to consider"
    :default 100 :parse-fn #(Integer/parseInt %)]
   ["-s" "--max-changeset-size MAX-CHANGESET-SIZE"
    "Maximum number of modules in a change set if it shall be included in a coupling analysis"
    :default 30 :parse-fn #(Integer/parseInt %)]
   ["-p" "--enforce-changeset-size ENFORCE-CHANGESET-SIZE"
    "Applies the -s value during the parsing stage. Normally -s is something just a few analyses care about."]
   ["-e" "--expression-to-match MATCH-EXPRESSION" "A regex to match against commit messages. Used with -messages analyses"]
   ["-sasd" "--temporal-by-author-and-day TEMPORAL-BY-AUTHOR-AND-DAY"
    "Instructs the tool to consider all commits during the same day by the same author as a single, logical commit"]
   ["-D" "--select-distinct-commits" "Filter to only use distinct commits based on date, author, message and entity."]
   ["-d" "--age-time-now AGE-TIME_NOW" "Specify a date as YYYY-MM-dd that counts as time zero when doing a code age analysis"]
   ["-f" "--exclude-files EXCLUDE_FILES" "A semicolon separated list of glob patterns specifying files to exclude from the analysis"]
   ["-k" "--known-authors KNOWN_AUTHORS" "A CSV file with the authors that will be included in the analysis (all other authors are filtered away)"]
   ["-y" "--team-map TEAM_MAP" "A CSV file with authors mapped to their teams; Runs the analyses on team level instead"]
   ["-u" "--augment-author-teams" "Augment the name of each author with their team. Useful for communicatin visualization."]
   ["-ut" "--unknown-team-name" "Specifies the default team name in case the user has not provided a mapping for a developer."]
   ["-fs" "--filter-specified-files FILTER_SPECIFIED_FILES" "Exclude all files except the given ones, which should be a CSV of name,code and optional columns."]
   ["-fsd" "--filter-start-date FILTER_START_DATE" "Filters all author dates before the given YYYY-MM-DD. Needed since the commit may have been merged or pathed at a later date."]
   ["-fed" "--filter-end-date FILTER_END_DATE" "Filters all author dates after the given YYYY-MM-DD."]
   ["-mex" "--message-extraction-pattern MESSAGE-EXTRACTION-PATTERN"
    "A regex used to extract the part of the commit header messagge that matches. Used in conjunction with --temporal-by-ticket-id to group multiple commits for coupling analyses."]
   ["-tti" "--temporal-by-ticket-id TEMPORAL-BY-TICKET-ID"
    "Instructs the tool to consider all commits with the same ticket ID, extracted by -mex, as a single, logical commit."]
   ["-pen" "--parsed-entity-namespace PARSED-ENTITY-NAMESPACE"
    "Adds the given string as the root namespace of each parsed entity. Typically used to group the data from different repository while avoiding name clashes and maintining a hierarchy"]
   ["-pte" "--patterns-to-exclude PATTERNS-TO-EXCLUDE"
    "Specify a semi-colon separated list of glob patterns to exclude specific content."]
   ["-h" "--help"]])

(defn- usage [options-summary]
  (string/join
   \newline
   ["This is part of the Empear AB tool suite, used to collect statistics from a VCS."
    "Version: 0.7.9-SNAPSHOT"
    ""
    "Usage: program-name -l log-file [options]"
    ""
    "Options:"
    options-summary
    "Please refer to the manual page for more informatio
n."]))

(defn- error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn- exit [status msg]
  (println msg)
  (System/exit status))

(defn- run-with-forced-output
  [log options]
  (app/run log options)
  (flush))

(defn run-with
  [args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
     (:help options) (exit 0 (usage summary))
     errors (exit 1 (error-msg errors)))
    :else
    (run-with-forced-output (:log options) options)))

(defn -main
  [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
     (:help options) (exit 0 (usage summary))
     errors (exit 1 (error-msg errors)))
    :else
    (try
      (app/run (:log options) options)
      (flush)
      (catch IllegalArgumentException e
        (.printStackTrace e)
        (println "Invalid argument: " (.getMessage e))
        (exit 1 (usage summary)))
      (catch Exception e ; this is our main recovery point
        (.printStackTrace e)
        (println "Error: " (.getMessage e))
        (exit 1 (usage summary))))))
