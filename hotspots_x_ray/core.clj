(ns hotspots-x-ray.core
  (:gen-class)
  (:require [hotspots-x-ray.x-rayer :as x-rayer]
            [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [clj-time.format :as f]
            [clojure.string :as string]
            [taoensso.timbre :as timbre]
            [hotspots-x-ray.diagnostics.performance :as diagnostics]
            [hotspots-x-ray.languages.parser :as parsers]
            [hotspots-x-ray.content-resolver :as resolver]
            [clojure.data.csv :as csv]))

(defn- write-to-named-file
  [p {:keys [analysis result]}]
  (let [out-file-name (str analysis ".csv")
        out-name (resolver/as-child-path p out-file-name)]
    (with-open [out-file (io/writer out-name)]
      (csv/write-csv out-file [(map name (:headers result))])
      (csv/write-csv out-file (:results result)))))

(defn- write-to-stdout
  [{:keys [analysis result]}]
  (println analysis)
  (csv/write-csv *out* [(:headers result)])
  (csv/write-csv *out* (:results result))
  (println))

(defn x-ray-with-defaults
  [{:keys [repo file date _lang outpath thresholds] :or {thresholds {}}} git-cmd]
  (diagnostics/with-timbre-exe-time-info
    (let [outputter (if (some? outpath) (partial write-to-named-file outpath) write-to-stdout)
          results (x-rayer/x-ray repo date file git-cmd thresholds)]
      (doseq [r results]
        (outputter r)))))

(def supported-parsers parsers/supported-parsers)

(def supports-file-type? parsers/supports-file-type?)

;;
;; Command Line API
;;

(def ^:private date-formatter (f/formatter "yyyy-MM-dd"))

(defn- valid-date?
  [s]
  (try
    (f/parse date-formatter s)
    (catch Exception _ false)))

(def cli-options
  [["-r" "--repo REPO" "The Git repository that contains the file to X-Ray."
    :parse-fn string/trim
    :validate [#(io/as-file %) "The repository doesn't exist."]]
   ["-f" "--file FILE" "The relative path (from the -r root) to the file you want to X-Ray."
    :parse-fn string/trim]
   ["-d" "--date DATE" "Run the analysis with this start data."
    :validate [valid-date? "The date must be given as YYYY-MM-DD at a future date"]]
   ["-o" "--outpath OUTPATH" "Write the result to files on the given path"]
   ["-l" "--lang LANG" (str "Force the language to analyze. Valid options: " (parsers/supported-parsers))]
   ["-h" "--help"]])

(defn- usage [options-summary]
  (string/join
   \newline
   ["This is part of the Empear AB tool suite, used to collect statistics from a VCS."
    "Version: 0.1-SNAPSHOT"
    ""
    "Usage: program-name -r any/git/repository/path -f src/the/file.java [options]"
    ""
    "Options:"
    options-summary]))

(defn- error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn- exit [status msg]
  (println msg)
  (System/exit status))

(def ^:const default-git-cmd "git") ; TODO: control via the command line?

(defn -main
  [& args]
  (timbre/set-level! :error)
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 (usage summary))
      errors (exit 1 (error-msg errors)))
    :else
    (try
      (x-ray-with-defaults options default-git-cmd)
      (flush)
      (System/exit 0) ; why do I need this one??
      (catch Exception e ; this is our main recovery point
        (.printStackTrace e)
        (println "Error: " (.getMessage e))
        (exit 1 (usage summary))))))
