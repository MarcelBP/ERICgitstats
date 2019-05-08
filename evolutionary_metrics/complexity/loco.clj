;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.complexity.loco
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.complexity.loco-rules :as rules]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.complexity.specs :as complexity-specs]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [evolutionary-metrics.mining.file-patterns :as patterns])
  (:import (java.nio.file Files)
           (net.sf.jmimemagic Magic MagicMatchNotFoundException)
           (java.io FileNotFoundException)))

;;; The loco module is a growing port of the functionality in cloc.
;;; We basically use the Lines Of Code as a proxy for complexity.
;;
;;; Design principles:
;;; ==================
;;;
;;; - Use a declarative style where we specify the rules for each
;;; langauge.
;;; - Unknown languages are reported; Don't attempt to parse them
;;; since it may be binary artifacts.

(defn extension-of
  [file-name]
  (re-find #"\.[^.]*$" file-name))

(defn extension-of-file
  [f]
  (extension-of (.getName f)))

(defn- git-repo-content?
  [f]
  (.contains (str f) "/.git/"))

(defn- file->relative
  "Since we need to match the name of the file with
   the info mined from the VCS (always relative)."
  [base-path f]
  (.getPath
    (.relativize (.toURI base-path)
                 (.toURI f))))

(defn- symbolic-link?
  [f]
  (Files/isSymbolicLink (.toPath f)))

(defn file-seq-without-links
  "We've seen at least one case where a symbolic link lead to
   an infinite recursion."
  [dir]
  (tree-seq
   (fn [^java.io.File f] (.isDirectory f))
   (fn [^java.io.File d] (seq (remove symbolic-link? (.listFiles d))))
   dir))

(defn- ignore-file?
  [f]
  (or (not (.canRead f))
      (.isDirectory f)
      (symbolic-link? f)
      (.isHidden f)
      (git-repo-content? f)
      (rules/ignore-extension? (extension-of-file f))))

(defn as-pattern-based-exclusion-filter
  [{:keys [patterns-to-exclude patterns-to-include]} to-relative-file-fn]
  (let [exclusion-filter-fn (patterns/make-black-list-filter patterns-to-exclude)
        inclusion-filter-fn (patterns/make-white-list-filter patterns-to-include)]
    (fn [name]
      (let [relative-name (to-relative-file-fn name)]
        (or (inclusion-filter-fn relative-name)
            (exclusion-filter-fn relative-name))))))

(defn- files-in
  [path {:keys [keep-specific-file-fn] :as options} to-relative-file-fn]
  (let [exclude-file? (as-pattern-based-exclusion-filter options to-relative-file-fn)
        file-candidates (filter (comp keep-specific-file-fn second patterns/de-scope to-relative-file-fn) (file-seq-without-links (io/file path)))]
    (remove #(or (exclude-file? %) (ignore-file? %)) file-candidates)))

(defn rule-for-file-name
  [file-name]
  (if-let [possible-extension (extension-of file-name)]
    (rules/rule-for possible-extension)
    (rules/rule-for file-name)))                            ; perhaps VERSION, Makefile, etc

(defn- rules-for
  [f]
  (rule-for-file-name (.getName f)))                        ; perhaps VERSION, Makefile, etc

(defn fallback-text-rule [file-name]
  (let [ext (extension-of file-name)]
    (rules/pure-text (or ext "Unknown"))))

(defn- always-false
  [_]
  false)

(defprotocol CodeStatsAccumulator
  (blanks [x] "Accumulates a blank code line.")
  (comments [x line] "Accumulates a comment code lines, which may be part of a multi-line comment")
  (codes [x line] "Accumulates a line of code.")
  (in-multi? [x])
  (start-multi-comment [x line])
  (end-multi-comment [x line])
  (stats [x] "Returns the accumulated statistics"))

(deftype LineCountCodeStatsAccumulator [blank-state comment-state code-state in-multi-state]
  CodeStatsAccumulator
  (blanks [x] (LineCountCodeStatsAccumulator. (inc blank-state) comment-state code-state in-multi-state))
  (comments [x line] (LineCountCodeStatsAccumulator. blank-state (inc comment-state) code-state in-multi-state))
  (codes [x line] (LineCountCodeStatsAccumulator. blank-state comment-state (inc code-state) in-multi-state))
  (in-multi? [x] in-multi-state)
  (start-multi-comment [x _line] (LineCountCodeStatsAccumulator. blank-state (inc comment-state) code-state true))
  (end-multi-comment [x _line] (LineCountCodeStatsAccumulator. blank-state (inc comment-state) code-state false))
  (stats [x] [blank-state comment-state code-state]))

(defn make-line-count-accumulator
  []
  (LineCountCodeStatsAccumulator. 0 0 0 false))

(defn reduce-stats-in
  [lines {:keys [blank? comment? multi-start? multi-end? :multi-markers-equal]
          :or   {multi-start? always-false multi-end? always-false}}
   code-stats-acc]
  (reduce
    (fn [stats-acc line]
      (cond
        (and (not multi-markers-equal) (multi-start? line) (multi-end? line)) (comments stats-acc line)
        (and (not (in-multi? stats-acc)) (multi-start? line)) (start-multi-comment stats-acc line)
        (and (in-multi? stats-acc) (multi-end? line)) (end-multi-comment stats-acc line)
        (blank? line) (blanks stats-acc)
        (or (in-multi? stats-acc) (comment? line)) (comments stats-acc line)
        :else (codes stats-acc line)))
    code-stats-acc
    lines))

(defn code-stats-in
  "Calculates code statistics for the given lines according to the specified rules.
   If no statistics accumulator is given, we provide a default one that simply sums up
   blanks, code lines and comment lines. The user is free to override this behaviour by
   providing their own accumulator."
  ([lines rules]
   (code-stats-in lines rules (make-line-count-accumulator)))
  ([lines rules code-stats-acc]
   (stats
    (reduce-stats-in lines rules code-stats-acc))))

(defn text-file? [f]
  (try
    (let [mime-type (.getMimeType (Magic/getMagicMatch f true true))]
      (and (string? mime-type)
           (not (nil? (re-find #"^text\/.*" mime-type)))))
    (catch MagicMatchNotFoundException e
      false)))

(defn- ignored-file
  "Returns a result indicating that given file should be ignored."
  [file]
  ["Ignored" (.getName file) 0 0 0])

(defn- content-stats
  "Parses the given file to calculate statistics about its content.
   The return value is a vector with the following information:
    language,filename,blank,comment,code"
  [f to-relative-file-fn {:keys [auto-detect-text-files] :as _options}]
  (if-let [rule (or (rules-for f)
                    (when (and auto-detect-text-files (text-file? f))
                      (fallback-text-rule (.getName f))))]
    (try
      (with-open [rdr (io/reader f)]
        (let [[blanks comments code] (code-stats-in (line-seq rdr) rule)]
          [(rule :language) (to-relative-file-fn f) blanks comments code]))
      (catch FileNotFoundException e
        ;; we don't want the single broken file (e.g. "aux.js" which is a reserved file name on Windows)
        ;; to stop whole analysis - let's just ignore that file
        (ignored-file f)))
    (ignored-file f)))

(defn- code-size
  [file-stat]
  (nth file-stat 4))

(defn- stats-for
  [path options to-relative-file-fn]
  (->>
    (files-in path options to-relative-file-fn)
    (map #(content-stats % to-relative-file-fn options))
    ;; sort by decreasing codesize and (secondary) alphabetically by name (case insensitive)
    (sort-by (juxt (comp - code-size)
                   (comp clojure.string/lower-case second)))))

(defn- file-name-extractor-from
  [path options]
  (as-> path $
        (java.io.File. $)
        (comp (patterns/scoper-from options) (partial file->relative $))))

(defn- as-filter
  [user-option]
  (-> user-option
      patterns/user-option->patterns
      patterns/patterns->combined-name-filter-fn))

(defn- filter-excluded-file-extensions-from
  [stats patterns-to-filter]
  (let [keep? (as-filter patterns-to-filter)]
    (filter (fn [[_language file-name]] (keep? file-name)) stats)))

(defn- filter-extensions-by-glob-patterns
  "Filters the statistics based on a set of glob patterns
   given in the options.
   The rationale is to filter files that we want to exclude from
   an analysis. For example, in Java we don't necessarily want XML files."
  [stats options]
  (if-let [user-option (:exclude-files options)]
    (filter-excluded-file-extensions-from stats user-option)
    stats))

(defn- ignored-file-extension?
  [stat]
  (= (first stat) "Ignored"))

(s/def ::language string?)
(s/def ::files-count nat-int?)
(s/def ::blank-lines nat-int?)
(s/def ::comment-lines nat-int?)
(s/def ::code-lines nat-int?)
(s/def ::statistics-by-language (s/coll-of
                                 (s/tuple ::language ::files-count
                                          ::blank-lines ::comment-lines ::code-lines)))

(s/fdef filtered-stats-for
        :args (s/cat :path ::core/filename
                     :options ::complexity-specs/summary-options)
        :ret (s/keys :req-un [::known ::ignored]))

(defn filtered-stats-for
  [path options]
  (let [all-statistics (stats-for path options (file-name-extractor-from path options))
        filtered-by-extension (filter-extensions-by-glob-patterns all-statistics options)]
    {:known   (remove ignored-file-extension? filtered-by-extension)
     :ignored (filter ignored-file-extension? filtered-by-extension)}))

(defn- sum-files
  [language fs]
  (reduce
    (fn [res f]
      (let [[lang files blanks comments code] res
            [_ _file f-blanks f-comments f-code] f]
        [lang (inc files) (+ blanks f-blanks) (+ comments f-comments) (+ code f-code)]))
    [language 0 0 0 0]
    fs))

(defn- sum-by-language
  [files-by-lang]
  (map #(sum-files % (files-by-lang %)) (keys files-by-lang)))

(defn- sort-on-max-code-size
  [rows]
  (sort-by (fn [[_ _ _ _ code]] code) > rows))

(s/fdef summarize-stats
        :args (s/cat :path ::core/filename
                     :options ::complexity-specs/summary-options)
        :ret ::statistics-by-language)

(defn summarize-stats
  [path options]
  (->>
    (stats-for path options (file-name-extractor-from path options))
    (group-by first)
    sum-by-language
    sort-on-max-code-size))

(defn- as-int
  [s]
  (Integer/parseInt s))

(defn- sum-rows
  [r1 r2]
  (map + r1 r2))

(defn- rows->map
  [rows]
  (->>
    (map (fn [[language & s]] [language (map as-int s)]) rows)
    (into {})))

(defn combine-summaries
  "Used to combine summary analyses from multiple repositories."
  [summaries]
  (->> summaries
       (map rows->map)
       (reduce (fn [acc next-summary]
                 (reduce (fn [acc1 [language & row]]
                           (merge acc1
                                  {language (sum-rows (get acc1 language [0 0 0 0]) (first row))}))
                         acc
                         next-summary)))
       (map (fn [[k v]] (into [k] v)))
       sort-on-max-code-size))

(defn- report-ignored-files
  [ignored {:keys [ignored-files-fn]}]
  (when ignored-files-fn
    (let [file-names (map second ignored)]
      (ignored-files-fn file-names))))

;; Scramble content to make reports anonymous with respect to file names.
;; This is useful in both trial versions and in pre-sale proof of concepts.

(def ^:private file-scramble-base-name "TrialFile")

(defn- scramble-file-name
  [i n]
  (let [real-name (re-find #"^(.*?)/?([\w\-]+)(\.[^.]*)$" n)
        [_ leading-path _name ext] real-name]
    (str leading-path "/" file-scramble-base-name (inc i) ext)))

(defn scramble-file-names
  [names]
  (map-indexed scramble-file-name names))

                        ;;; API

(s/fdef detailed-stats
        :args (s/cat :path ::core/filename
                     :options ::complexity-specs/detailed-stats-options
                     :out-file-name ::core/filename)
        :ret nil?)

(defn detailed-stats
  [path options out-file-name]
  (let [{:keys [known ignored]} (filtered-stats-for path options)]
    (report-ignored-files ignored options)
    (with-open [out-file (io/writer out-file-name)]
      (csv/write-csv out-file [["language" "filename" "blank" "comment" "code"]])
      (csv/write-csv out-file known))))

(s/fdef summary
        :args (s/cat :path ::core/filename
                     :options ::complexity-specs/summary-options
                     :out-file-name ::core/filename)
        :ret nil?)

(defn summary
  [path options out-file-name]
  (let [res (summarize-stats path options)]
    (with-open [out-file (io/writer out-file-name)]
      (csv/write-csv out-file [["language" "files" "blank" "comment" "code"]])
      (csv/write-csv out-file res))))

(s/fdef combine-multiple-summaries
        :args (s/cat :summaries ::complexity-specs/summaries
                     :out-file-name ::core/filename)
        :ret nil?)

(defn combine-multiple-summaries
  [summaries out-file-name]
  (let [combined (->> summaries (map shared/read-csv-sans-header-from) combine-summaries)]
    (with-open [out-file (io/writer out-file-name)]
      (csv/write-csv out-file [["language" "files" "blank" "comment" "code"]])
      (csv/write-csv out-file combined))))

(s/fdef scramble-all-file-names
        :args (s/cat :complexity-file-name ::core/filename
                     :out-file-name ::core/filename)
        :ret nil?)

(defn scramble-all-file-names
  [complexity-file-name out-file-name]
  (let [content (shared/read-csv-sans-header-from complexity-file-name)
        names (map second content)
        scrambled-names (scramble-file-names names)
        translations (into {} (map vector names scrambled-names))
        scrambled-content (map (fn [[lang _name blank comment code] new-name] [lang new-name blank comment code]) content scrambled-names)]
    (with-open [out-file (io/writer out-file-name)]
      (csv/write-csv out-file [["language" "filename" "blank" "comment" "code"]])
      (csv/write-csv out-file scrambled-content)
      translations)))
