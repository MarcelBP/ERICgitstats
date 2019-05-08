(ns evolutionary-metrics.parsers.git-commit-parser
  (:import [java.io Reader])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [evolutionary-metrics.parsers :as parsers]
            [evolutionary-metrics.parsers.git-rename-detection :as rename-detector]
            [evolutionary-metrics.parsers.unicode :as unicode]
            [taoensso.timbre :as log]))

;;; This module is responsible for parsing a git log file. This parser differs
;;; from the other ones as we attempt to optimize the memory consumption by
;;; not only reading lines one by one but also by parsing them individually.

(def ^:const parsed-git-entry-headers
  [["author-email"] ["date"] ["basic-date-time"]
   ["loc-deleted"] ["loc-added"] ["rev"] ["author"] ["extraction-name"] ["extracted?"] ["entity"] ["message"]])

(defn parse-time-field
  [time-field]
  {:short-date      (first (string/split time-field #"T"))
   :basic-date-time time-field})

(defn- header-fields-from
  [line]
  (string/split (string/replace-first line "--" "") #"\t"))

(defn- commit-hash-from-header
  [h]
  (nth h 0))

(defn- author-email-from-header
  [h]
  (nth h 3 "unknown@email.com"))

(defn- message-from
  [header-fields]
  (string/join "\t" (drop 4 header-fields)))

(defn- parse-header
  [minimimal-message-fn [line]]
  (try
    (let [header-fields (header-fields-from line)]
      (when (< (count header-fields) 3)                     ; hash, date and author are mandatory - we can live without the rest
        (throw (IllegalArgumentException. (str "Header line does not have all required fields: " line))))
      (let [{:keys [short-date basic-date-time]} (parse-time-field (nth header-fields 1))]
        {:rev             (commit-hash-from-header header-fields)
         :date            short-date
         :author          (nth header-fields 2)
         :author-email    (author-email-from-header header-fields)
         :message         (-> header-fields message-from minimimal-message-fn)
         :basic-date-time basic-date-time}))
    (catch IllegalArgumentException e
      (throw e))
    (catch Exception ex
      (throw (IllegalArgumentException. (str "Failed to parse header: " line) ex)))))

(defn- parse-single-file-change
  [handle-nonascii-paths? [line]]
  (let [parts (string/split line #"\t")]
    (if (= (count parts) 3)                                 ;we have a path line
      (let [[_ _ s] parts]
        (if (and handle-nonascii-paths? (unicode/quotes-present? s))
          ;; compose unicode and remove quotes (should be unusual)
          (update parts 2 (comp unicode/trim-quotes unicode/compose))
          ;; leave path unchanged (should happen for vast majority of cases)
          parts))
      (throw (IllegalArgumentException. (str "The change within a Git commit body does not match the expected format. Line: " line))))))

(defn- parse-changes
  [all-file-names header-entry acc change]
  (let [[added deleted entity] change
        extraction-info (rename-detector/file-extracted-from-existing-file? all-file-names entity)]
    (conj acc (merge header-entry
                     {:entity      entity
                      :loc-added   added
                      :loc-deleted deleted}
                     extraction-info))))

(def ^:private header-regex #"^--([a-f]|\d)+\s+\d{4}-\d{2}-\d{2}T\d{2}:\d{2}")

(defn- header?
  [line]
  (re-find header-regex line))

(defn- matches-commit-hash?
  [h line]
  (= h line))

(defn- combine-multi-line-message
  [minimimal-message-fn multi first-part]
  (->> multi
       (map first)
       (map minimimal-message-fn)
       (map string/trim-newline)
       (remove string/blank?)
       (map string/trim)
       distinct
       (string/join " ")
       (str first-part " ")
       string/trim))

(defn- changes-after-header-marker
  "We use the commit has as a unique end of header marker.
   Here we read out the actual changes while ensuring we
   have the marker (catch test data that's on legacy format"
  [n-header-lines lines-left hash]
  (let [rest-lines (drop n-header-lines lines-left)
        expected-hash (first rest-lines)]
    (when-not (matches-commit-hash? hash (first expected-hash))
      (throw (IllegalArgumentException. (str "Wrong log format: The header isn't terminated with the hash " hash))))
    (drop 1 rest-lines)))

(defn- read-headers-from
  [entry]
  (filter (fn [[line]] (header? line)) entry))

(defn- count-headers
  "We need to adjust for the fact that we have a commit hash at the end of each header."
  [header-lines]
  (dec (* 2 (count header-lines))))

(defn- make-parser
  [minimimal-message-fn handle-nonascii-paths?]
  (fn [entities-cache entry]                                ; we must expect multiple headers here - pick the last one!
    (let [headers (read-headers-from entry)
          header-to-use (first (reverse headers))
          {:keys [rev] :as parsed-header} (parse-header minimimal-message-fn header-to-use)
          rest-of-entry (drop (count-headers headers) entry)
          multi-line-message (take-while (fn [[line]] (not (matches-commit-hash? rev line))) rest-of-entry)
          complete-header (update parsed-header :message (partial combine-multi-line-message minimimal-message-fn multi-line-message))
          changes (changes-after-header-marker (count multi-line-message) rest-of-entry rev)
          parsed-changes (map (partial parse-single-file-change handle-nonascii-paths?) changes)
          all-file-names (->> parsed-changes (map last) set)]
      (reduce (partial parse-changes all-file-names complete-header)
              entities-cache
              parsed-changes))))

(defn- raise-tokenizer-error
  [state line]
  (throw (IllegalArgumentException. (str "Unexpected input in the state " state ". Input: " line))))

(defn- extend-when-complete
  "Keep each line wrapped in its own vector so that
   we're able to join them with a delimiter for the grammar."
  [entities-cache next-line entry-acc {:keys [tokenizer-state hash] :as _state} parse-fn]
  (case tokenizer-state

    :idle (if (header? next-line)
            [entities-cache (conj entry-acc [next-line]) {:tokenizer-state :parse-header
                                                          :hash            (->> next-line header-fields-from commit-hash-from-header)}]
            (raise-tokenizer-error tokenizer-state next-line))

    :parse-header (if (matches-commit-hash? next-line hash)
                    [entities-cache (conj entry-acc [next-line]) {:tokenizer-state :parsed-header}]
                    [entities-cache (conj entry-acc [next-line]) {:tokenizer-state :parse-header :hash hash}])

    :parsed-header (if (header? next-line)
                     [entities-cache (conj entry-acc [next-line]) {:tokenizer-state :parse-header
                                                                   :hash            (->> next-line header-fields-from commit-hash-from-header)}]
                     (if (string/blank? next-line)          ; entry complete
                       [(parse-fn entities-cache entry-acc) [] {:tokenizer-state :idle}]
                       [entities-cache (conj entry-acc [next-line]) {:tokenizer-state :parsed-header}]))))

(defn- complete-the-rest-in
  "Constructs the final entry in the version-control log.
   The entries are separated by a token (e.g. blank line) _except_ for
   the last one that doesn't get a trailing newline."
  [entry-acc entities-cache parse-fn]
  (if (empty? entry-acc)
    entities-cache
    (parse-fn entities-cache entry-acc)))

(defn as-entry-tokens
  [parse-fn lines]
  (loop [lines-left lines
         entry-acc []
         tokenizer-state {:tokenizer-state :idle}
         entities-cache []]
    (if (empty? lines-left)
      (complete-the-rest-in entry-acc entities-cache parse-fn)
      (let [next-line (first lines-left)
            [updated-entities-cache updated-acc updated-tokenizer-state] (extend-when-complete entities-cache
                                                                                               next-line
                                                                                               entry-acc
                                                                                               tokenizer-state
                                                                                               parse-fn)]
        (recur (rest lines-left) updated-acc updated-tokenizer-state updated-entities-cache)))))

(defn- pad-when-value
  [v]
  (if v (str " " v)))

(defn- prevent-overlap-between
  [words defects]
  (if (= words defects)
    (pad-when-value words)
    (str (pad-when-value words) (pad-when-value defects))))

(def ^:private minimal-message-text "")

(defn make-message-shortener
  "A commit message could be really long and there's a lot of space -- and memory -- to
   save by stripping it as short as possible.
   The key is to maintain only the information needed in sub-sequent analyses and there
   are few analyses that depend upon the message."
  [{:keys [message-extraction-pattern expression-to-match pair-programming-pattern hotspot-defect-commit-pattern]}]
  (if (or message-extraction-pattern expression-to-match pair-programming-pattern hotspot-defect-commit-pattern)
    (let [p-ticket (re-pattern (or message-extraction-pattern "(?!)"))
          p-defects (re-pattern (or hotspot-defect-commit-pattern "(?!)"))
          p-words (re-pattern (str "(?i)" (or expression-to-match "(?!)")))
          p-pair-prog (re-pattern (or pair-programming-pattern "(?!)"))]
      (fn [message]
        (let [m-ticket (first (re-find p-ticket message))   ; always a match group
              m-defects (re-find p-defects message)
              m-words (re-find p-words message)
              m-word-defects (prevent-overlap-between m-words m-defects)
              m-pair-prog (first (re-find p-pair-prog message))] ; always one or two match groups
          (str m-ticket  m-word-defects
               (pad-when-value m-pair-prog))))) ; safe since str accepts nil
    (constantly minimal-message-text)))

(defn- parse-from
  "Expected to be invoked in a with-open context."
  [options rdr]
  (let [{:keys [handle-nonascii-paths?]} options
        minimimal-message-fn (make-message-shortener options)
        parse-fn (make-parser minimimal-message-fn handle-nonascii-paths?)]
    (as-entry-tokens parse-fn (line-seq rdr))))

(s/def ::parse-options (s/keys :opt-un [::message-extraction-pattern
                                        ::expression-to-match
                                        ::hotspot-defect-commit-pattern
                                        ::handle-nonascii-paths?]))

(defn parse-from-reader
  [options rdr]
  (log/debug "Parse Git evolutionary data")
  (let [result (parse-from options rdr)]
    (if (seq result)
      result
      [])))

(s/fdef parse-from-reader
        :args (s/cat :options ::parse-options
                     :rdr #(instance? Reader %))
        :ret ::parsers/vcs-log)
