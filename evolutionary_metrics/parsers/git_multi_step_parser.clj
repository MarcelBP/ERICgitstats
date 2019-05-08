
(ns evolutionary-metrics.parsers.git-multi-step-parser
  (:require [evolutionary-metrics.parsers.git-commit-parser :as commit-parser]
            [evolutionary-metrics.parsers :as parsers]
            [codescene.analysis.specs :as analysis-specs]
            [evolutionary-metrics.complexity.loco-rules :as extension-rules]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.parsers.git-rename-detection :as rename-detector]
            [evolutionary-metrics.mining.file-patterns :as patterns]
            [evolutionary-metrics.app.exclude-commits :as exclude-commits]
            [taoensso.timbre :as log])
  (:import (java.io StringReader BufferedReader)))

;; The algorithm:
;; 1. Parse without any rename detection, only filter on file endings (make it efficient like ends-with? ...).
;; 2. Perform rename detection, add an extra column that indicates "inconclusive initial file size".
;; 3. Filter based on black- and white listed patterns.
;; 4. Resolve inconclusive initial file sizes, when configured.
;; 5. Delete the intermediate files.
;;
;; Additional optimizations include:
;; 1. As soon as we've parsed a complete commit we write it to disk as CSV.
;; 2. The rename detection step can now be performed by reading the CSV line by line and
;;    writing down the result immediately.

(defn- make-extension-filter
  "The user only specifies a set of file extensions to include.
   It's our task to convert them to glob patterns that matches all
   instances of those extensions, no matter at what directory level."
  [{:keys [exclusionfilter]}]
  (if exclusionfilter
    (let [patterns (as-> exclusionfilter f
                        (string/split f #"(^\*\.)|(;\*\.)")
                        (remove empty? f)
                        (map #(re-pattern (str "\\." % "}?$")) f))]
      (log/debug "Exclude files with the following extensions: " exclusionfilter)
      (fn [entity]
        (some (fn [p] (re-find p entity)) patterns)))
    (constantly false)))

(defn- extension-of
  "Fetches the file extension while taking possible rename patterns into account"
  [file-name]
  (or
    (some->> file-name
             (re-find #"(\.[^.^}]*)}?$")
             second)
    "")) ; e.g. Makefile

(defn- filter-binary-content
  [commits]
  (log/debug "Filter known binary content")
  (remove (comp extension-rules/ignore-extension? extension-of :entity) commits))

(defn- filter-excluded-file-extensions
  [filter-extension? commits]
  (log/debug "Filter excluded file extensions")
  (remove (comp filter-extension? :entity) commits))

(defn- resolve-names-for-revision
  [{:keys [rename-cache entities]} {:keys [entity extracted?] :as change}]
  (let [{:keys [rename-cache name]} (rename-detector/adjusted-name-for rename-cache extracted? entity)]
    {:rename-cache rename-cache
     :entities (conj entities (merge change {:entity name}))}))

(defn- resolve-renames
  [commits]
  (log/debug "Resolve moved and renamed content (detailed step)")
  (->> commits
       (reduce resolve-names-for-revision {:rename-cache rename-detector/empty-rename-cache :entities []})
       :entities))

(defn- keep-white-listed-content
  [{:keys [patterns-to-include]} commits]
  (if patterns-to-include
    (let [bf (patterns/make-white-list-filter patterns-to-include)]
      (log/debug "White list content")
      (remove (comp bf :entity) commits))
    commits))

(defn- remove-black-listed-content
  [{:keys [patterns-to-exclude]} commits]
  (if patterns-to-exclude
    (let [bf (patterns/make-black-list-filter patterns-to-exclude)]
      (log/debug "Remove black listed content")
      (remove (comp bf :entity) commits))
    commits))

(defn- resolve-initial-file-size
  [{:keys [lookup-copied-content resolve-loc-for]} commits]
  (map #(dissoc % :extracted? :extraction-name)
       (if (and lookup-copied-content (some? resolve-loc-for))
         (map (fn [{:keys [extracted? extraction-name] :as c}]
                (if extracted?
                  (let [code-churn {:loc-added (:loc-added c)
                                    :loc-deleted (:loc-deleted c)}
                        resolved-churn (resolve-loc-for {:rev (:rev c) :code-churn code-churn} extraction-name)]
                    (merge c resolved-churn))
                  c))
              commits)
         commits)))

(defn- encoding-from
  [options]
  (get options :input-encoding "UTF-8"))

(defn- add-repository-scope
  [options commits]
  (let [scoper (patterns/scoper-from options)]
    (map (fn [c] (update c :entity scoper)) commits)))

(defn- trace-parse-completion
  [commits]
  (log/debug "Parsing completed")
  commits)

(defn- parse-from-reader
  [options reader write-intermediate-results-for-renaming!]
  (let [filter-extension? (make-extension-filter options)
        write-intermediate-results-for-renaming! (or write-intermediate-results-for-renaming!
                                                     identity)]
    (->> reader
         (commit-parser/parse-from-reader options)
         write-intermediate-results-for-renaming!
         filter-binary-content
         (exclude-commits/by-commit-hash options)
         (filter-excluded-file-extensions filter-extension?)
         resolve-renames
         (add-repository-scope options)
         (keep-white-listed-content options)
         (remove-black-listed-content options)
         (resolve-initial-file-size options)
         trace-parse-completion)))

;;
;; API
;;

(s/def ::lookup-copied-content boolean?)
(s/def ::resolve-loc-for ifn?)

(s/def ::parse-options (s/keys :opt-un [::lookup-copied-content
                                        ::resolve-loc-for
                                        ::input-encoding
                                        ::exclusionfilter
                                        ::patterns-to-exclude
                                        ::patterns-to-include
                                        ::parsed-entity-namespace
                                        ::analysis-specs/exclude-commits]))

(defn parse-from-file
  "Transforms the given input git log into an
  Incanter dataset suitable for the analysis modules."
  [input-file-name options & [write-intermediate-results-for-renaming! :as var-args]]
  (with-open [rdr (clojure.java.io/reader
                    input-file-name
                    :encoding (encoding-from options))]
    (doall
     (parse-from-reader options rdr write-intermediate-results-for-renaming!))))

(s/fdef parse-from-file
        :args (s/cat :input-file-name ::core/filename
                     :options ::parse-options
                     :var-args (s/* fn?))
        :ret ::parsers/vcs-log)

(defn parse-from-string
  "Entry functions for out unit tests. Expects a string representing the
   raw Git log as input."
  [log-text options & [write-intermediate-results-for-renaming! :as var-args]]
  (with-open [rdr (BufferedReader. (StringReader. log-text))]    (parse-from-reader options rdr write-intermediate-results-for-renaming!)))

(s/fdef parse-from-string
        :args (s/cat :log-text string?
                     :options ::parse-options
                     :var-args (s/* fn?))
        :ret ::parsers/vcs-log)
