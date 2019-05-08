(ns evolutionary-metrics.mining.file-patterns
  (:require [clojure.string :as str]
            [stch.glob :as glob]
            [clojure.java.io :as io])
  (:import [java.io File]))

;;; We want to exclude certain files during the data mining.
;;; For example, we don't want to include generated build artifacts like
;;; exe- or jar-files.
;;; Similarly, the user may chose to exclude entire parts of the system from
;;; an analysis (external code, sub-systems that aren't within their scope, etc).
;;;
;;; This module encapsulates the routines for excluding such files based on user
;;; provided glob-patterns.
;;;

(defn first-segment
  [path]
  (-> path
      (clojure.string/split #"/") ; take care - this depends on context - we know we're only called with unix paths. Also: returns empty string with absolute paths.
      first))

(defn final-segment
  [path]
  (let [guaranteed-unix-style (.getPath (.toURI (io/file path)))]
    (last (clojure.string/split guaranteed-unix-style #"/"))))

(defn except-final-segment
  [path]
  (->> (str/split path #"/")            ; unix-paths only: see `first-segment`
       drop-last
       (str/join #"/")))

(defn add-repo-name
  [repo name]
  (str repo "/" name))

(defn scoper-from
  [{:keys [parsed-entity-namespace]}]
  (if parsed-entity-namespace
    (fn [name]
      (add-repo-name parsed-entity-namespace name))
    identity))

(defn make-scope->repo-lookup
  [repo-paths]
  (->> repo-paths
       (map (fn [path] [(final-segment path) path]))
       (into {})))

(defn de-scope
  "We augment all file names with a virtual root scope in order to
   maintain a hierarchy for multiple repos.
   When running git commands, for example to extract historic complexity, we
   need to de-scope each name again. The virtual root is kept as a key to
   look-up the corresponding physical repository path."
  [name]
  (let [scope (first-segment name)
        real-name (clojure.string/replace-first name (re-pattern (str scope "/")) "")]
    [scope real-name]))

(defn user-option->patterns
  [u]
  (str/split u #";"))

(defn- patterns->specific-filter-fn
  [filter-fn pattern-descriptions]
  (let [patterns (map glob/compile-pattern pattern-descriptions)]
    (fn [name]
      (filter-fn #(glob/match-glob % name) patterns))))

(def patterns->combined-name-filter-fn (partial patterns->specific-filter-fn not-any?))

(def patterns->match-name-filter-fn (partial patterns->specific-filter-fn some))

(def ^:private validation-data "this/is/just/some/pattern.c")

(defn- valid-pattern-expression?
  [pattern]
  (or (empty? pattern)
      (re-matches #"\*\.\w+(\.\w+)?" pattern)))

(defn valid-file-extension-pattern?
  "Unfortunately we can only do quite basic validation.
   We need to cover the rest with documenation."
  [pattern-expression]
  (try
    (let [patterns (user-option->patterns pattern-expression)]
      (when (every? valid-pattern-expression? patterns)
        (let [matcher (patterns->combined-name-filter-fn patterns)]
          (matcher validation-data) ; make sure we invoke it as well
          true)))
    (catch Exception _ false)))

(defn valid-glob-patterns?
  [pattern-expressions]
  (try
    (let [patterns (user-option->patterns pattern-expressions)
          matcher (patterns->combined-name-filter-fn patterns)]
      (matcher validation-data)
      true)
    (catch Exception _ false)))

(defn filter-from-semicolon-separated-list
  [semicolon-separated-patterns]
  (patterns->combined-name-filter-fn
   (user-option->patterns semicolon-separated-patterns)))

(defn make-pattern-matcher-for
  [patterns-to-include]
  (patterns->match-name-filter-fn
   (user-option->patterns patterns-to-include)))

(defn make-black-list-filter
  [patterns-to-exclude]
  (if (some? patterns-to-exclude)
    (comp not (filter-from-semicolon-separated-list patterns-to-exclude))
    (constantly false)))

(defn make-white-list-filter
  [patterns-to-include]
  (if (some? patterns-to-include)
    (comp not (make-pattern-matcher-for patterns-to-include))
    (constantly false)))
