;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.mining.vcs
  (:require [clojure.java.shell :as shell]
            [me.raynes.conch :as conch]                     ; to get the stdout from a shell command as a lazy seq
            [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.mining.specs :as mining-specs]
            [evolutionary-metrics.parsers.unicode :as unicode]
            [taoensso.timbre :as log]
            [clj-time.core :as t]
            [clj-time.format :as tf]))

;;; This module encapsulates all our interactions with Git.

(defn- fatal-error
  [code err args]
  (throw (Exception. (str "Failed to execute git command from <" args ">. Exit code: " code " Reason: " err))))

(defn run-failable-git-cmd
  [git-cmd & args]
  (let [{:keys [exit out err]} (apply shell/sh git-cmd args)]
    [exit out err]))

(defn- run-git-cmd-with-expected-success
  [git-cmd & args]
  (let [[exit out err] (apply run-failable-git-cmd git-cmd args)]
    (if (= 0 exit)
      out
      (fatal-error exit err (apply str args)))))

(s/fdef to-short-hash
        :args (s/cat :git-cmd ::mining-specs/git-exe-command
                     :repo ::mining-specs/repository-path
                     :h ::mining-specs/git-commit-hash)
        :ret ::mining-specs/git-commit-hash)

(defn to-short-hash
  "Note: we need to run it in the repository since the short hash varies
   depending on the repository content (minimizes risk for collisions)."
  [git-cmd repo h]
  (let [[exit-code out _] (run-failable-git-cmd git-cmd "rev-parse" "--short" h :dir repo)]
    (if (zero? exit-code)
      (string/trim-newline out)
      h)))

(defn check-git-version
  "Returns the version of the given command, expected
   to be Git, an empty string if the command isn't valid."
  [git-cmd]
  (try
    (let [{:keys [exit out _err]} (shell/sh git-cmd "--version")]
      (if (= 0 exit)
        out
        ""))
    (catch Exception e
      "")))

(defn git-head-revision-in
  [git-client repo-path]
  (let [{:keys [exit out err]} (shell/sh git-client "rev-parse" "HEAD" :dir repo-path)]
    (if (zero? exit)
      (clojure.string/trim-newline out)
      (do
        (log/error "Analysis task failed to fetch the HEAD revision for the repo "
                      repo-path ". Defaulting to HEAD. Reason: " err)
        "HEAD"))))

(defn fetch-changeset-from-specific-remote
  "This function is typically used in a Gerrit setting where
   the staging area (i.e. the commits to analyse) are located
   on a separate server. Hence, we need to provide that server URL and
   request a specific change set."
  [git-cmd origin-url change-ref repo]
  (run-git-cmd-with-expected-success git-cmd "fetch" origin-url change-ref :dir repo))

(s/fdef update-git-repo
        :args (s/cat
                :git-cmd ::mining-specs/git-exe-command
                :subcommand #{:pull :fetch}
                :prune-removed-remote-branches boolean?
                :repo ::mining-specs/repository-path))

(defmulti update-git-repo (fn [git-cmd subcommand prune-removed-remote-branches repo] subcommand))

(defmethod update-git-repo :pull
  [git-cmd _ prune-removed-remote-branches repo]
  (if prune-removed-remote-branches
    (run-git-cmd-with-expected-success git-cmd "pull" "--prune" :dir repo)
    (run-git-cmd-with-expected-success git-cmd "pull" :dir repo)))

(defmethod update-git-repo :fetch
  [git-cmd _ _prune-removed-remote-branches repo]
  (let [{:keys [out exit _err]} (shell/sh git-cmd "remote" :dir repo)
        remotes (string/split (string/trim out) #"\n")]
    (cond
      (or (pos? exit) (string/blank? out))
      (throw (ex-info "Could not detect remote branch, could not update with 'git fetch'."
                      {:dir repo}))

      (> (count remotes) 1)
      (throw (ex-info "More than one remote detected. Cannot run 'git fetch'."
                      {:dir     repo
                       :remotes remotes}))

      ::ok
      (run-git-cmd-with-expected-success git-cmd "fetch" (first remotes) :dir repo))))

(def ^{:private true :const true} pretty-format-flag "--pretty=format:--%h\t%ad\t%aN\t%aE\t%B%n%h")

(defn mine-deleted-content
  [git-cmd repo log-start-date]
  (run-git-cmd-with-expected-success git-cmd "log" "-M" "-C" "--diff-filter=D" "--numstat" "-w" "--ignore-blank-lines" "--date=iso-strict" (str "--after=" log-start-date) pretty-format-flag :dir repo))

(defn mine-git-with-rename-detection
  [git-cmd repo log-start-date rename-limit]
  (run-git-cmd-with-expected-success git-cmd
                                     "-c"
                                     (str "diff.renameLimit=" rename-limit)
                                     "log" "-M" "-C" "--numstat" "-w" "--ignore-blank-lines" "--date=iso-strict" (str "--after=" log-start-date) pretty-format-flag :dir repo))


(defn mine-complete-git-log-with-rename-detection
  "Like the other mining commands except we always take the full history.
   This log is typically used for knowledge mining that requires a complete history to
   be accurate."
  [git-cmd repo rename-limit]
  (run-git-cmd-with-expected-success git-cmd
                                     "-c"
                                     (str "diff.renameLimit=" rename-limit)
                                     "log" "-M" "-C" "--numstat" "-w" "--ignore-blank-lines" "--date=iso-strict" pretty-format-flag :dir repo))

(defn- mine-single-commit
  [git-cmd repo c]
  (run-git-cmd-with-expected-success git-cmd
                                     "log"
                                     c
                                     "-1"
                                     "-M"
                                     "-C"
                                     "--numstat"
                                     "-w"
                                     "--ignore-blank-lines"
                                     "--date=iso-strict"
                                     pretty-format-flag
                                     :dir repo))

(s/fdef mine-commit-range
        :args (s/cat :git-cmd ::mining-specs/git-exe-command
                     :repo ::mining-specs/repository-path
                     :commits ::mining-specs/git-revisions)
        :ret ::mining-specs/mined-git-log)

(defn mine-commit-range
  "Generates a git log, with rename detection enabled, for the range of specified commits (could be one, of course)."
  [git-cmd repo commits]
  (map (partial mine-single-commit git-cmd repo) commits))

(defn commits-in-range
  [git-cmd repo first-commit last-commit]
  (->>
    (run-git-cmd-with-expected-success git-cmd
                                       "log"
                                       "--pretty=format:%h"
                                       (str first-commit ".." last-commit)
                                       :dir repo)
    string/split-lines))

(defn- as-colon-separated
  [f s]
  (str f ":" s))

(defn- as-span
  [log-results]
  (let [first-commit (first log-results)
        last-commit (first (reverse log-results))]
    (as-colon-separated first-commit last-commit)))

(def ^:private git-short-date-format "--date=short")
(def ^:private git-rfc-date-format "--date=rfc")

(defn- raw-log-with-single-column-result
  [git-cmd repo format date-format & optional-args]
  (let [mandatory-args [git-cmd "log" date-format (str "--pretty=format:" format)]
        with-optional-args (into mandatory-args optional-args)
        all-args (into with-optional-args [:dir repo])]
    (apply run-git-cmd-with-expected-success all-args)))

(defn git-repo-timespan
  "Returns a string like 2014-07-01:2015-08-04 that
   specifies the time span of all commits in the
   repository"
  [git-cmd repo]
  (-> (raw-log-with-single-column-result git-cmd repo "%ad" git-short-date-format "--reverse")
      string/split-lines
      as-span))

(def as-start-date #(format "--after=%sT00:00:00" %))

(defn git-repo-revision-span
  "Returns a string like ac1da45:dd875c9 that
   specifies the first and last commit in the repository
   from the given start date."
  [git-cmd repo start-date]
  (-> (raw-log-with-single-column-result git-cmd repo "%h" git-short-date-format (as-start-date start-date) "--reverse")
      string/split-lines
      as-span))

(defn- ensure-unicode [paths]
  (map
    (fn [path]
      (if (unicode/quotes-present? path)
        ;; compose unicode and remove quotes (should be unusual)
        (-> path
            unicode/compose
            unicode/trim-quotes-simple)
        ;; leave path unchanged (should happen for vast majority of cases)
        path))
    paths))

(defn repository-content
  [git-cmd handle-nonascii-paths? repo]
  (let [[exit out err] (run-failable-git-cmd git-cmd "ls-files" :dir repo)]
    (if (= exit 0)
      (let [paths (clojure.string/split-lines out)]
        [true (if handle-nonascii-paths?
                (ensure-unicode paths)
                paths)])
      [false err])))

(s/fdef repository-content
        :args (s/cat :git-cmd ::mining-specs/git-exe-command
                     :handle-nonascii-paths? boolean?
                     :repo ::mining-specs/repository-path)
        :ret (s/or
               :success (s/tuple boolean? ::mining-specs/filenames)
               :failure (s/tuple boolean? ::mining-specs/git-error-message)))

(defn last-modification-to-file-relative-commit
  "This is used for the notes. We have the HEAD at the time the analysis was run, and
   now we need to fetch the last commit _prior_ to that which touched the given file.
   We're running a command like git rev-list -1 <analysis-base-commit> <filename>, for example
   git rev-list -1 fa2ef0c4cc5e05375672b9905ede22231be43272 pom.xml"
  [git-cmd head-commit repo-path file-name]
  (let [[exit out err] (run-failable-git-cmd
                         git-cmd
                         "rev-list"
                         "-1" ; only one commit of interest
                         "--no-merges" ; last commit that actually touched the file.
                         head-commit
                         file-name
                         :dir repo-path)]
    (if (= exit 0)
      {:success? true
       :rev (string/trim-newline out)}
      (do
        (log/warn "Git mining: Failed to fetch last revision of the file " file-name " in the repo " repo-path " relative the revision " head-commit ", reason: " err)
        {:success? false
         :rev head-commit}))))

(s/fdef last-modification-to-file-relative-commit
        :args (s/cat :git-cmd ::mining-specs/git-exe-command
                     :head-commit ::mining-specs/rev
                     :repo-path ::mining-specs/repository-path
                     :file-name ::mining-specs/name)
        :ret (s/keys :req-un [::success? ::mining-specs/rev]))

(defn show-file-version-at-revision
  "Returns the file as it existed in the given revision."
  [git-cmd repo file]
  (let [{:keys [rev name]} file
        [exit out err] (run-failable-git-cmd
                         git-cmd "show" (str rev ":" name) :dir repo)]
    (if (= exit 0)
      [true out]
      [false err])))

(s/fdef show-file-version-at-revision
        :args (s/cat :git-cmd ::mining-specs/git-exe-command
                     :repo ::mining-specs/repository-path
                     :file ::mining-specs/file-in-revision)
        :ret (s/or
               :success (s/tuple boolean? ::mining-specs/file-content)
               :failure (s/tuple boolean? ::mining-specs/git-error-message)))

(defn historic-file-version-of
  "Returns the file as it existed in the given revision.
   Note that we need to take some care here: in case a file is deleted in
   one revision (and later re-added...) we'll get an error from 'show' that
   the file exists on disk but not in the given commit!
   This is solved by returning a tuple where the first element specifies success."
  [git-cmd repo {:keys [date rev name] :as file}]
  (let [[success result] (show-file-version-at-revision git-cmd repo file)]
    (if success
      [true {:name name :date date :rev rev :content result}]
      [false {:name name :rev rev :date date :error result}])))

(defn lazy-historic-file-version
  [git-cmd repo {:keys [date rev name] :as file}]
  (try
    (conch/let-programs [user-git git-cmd]
                        [true {:name    name
                               :date    date
                               :rev     rev
                               :content (user-git "show" (str rev ":" name) {:dir repo
                                                                             :seq true})}])
    (catch Exception e
      [false {:name name :rev rev :date date :error (ex-data e)}])))

(defn as-commits
  [date-parser log-output]
  (->> log-output
       string/split-lines
       (partition-by string/blank?)
       (filter #(= 2 (count %)))                            ; strip both blank lines _and_ the case found in ENTERPRISE-306 where a submodule was replaced by a dir
       (map (fn [[date-rev file-name]]
              (let [[date rev] (string/split date-rev #"--")]
                {:date (date-parser date) :rev rev :name file-name})))))

(defn as-authored-commits
  [date-parser log-output]
  (->> log-output
       string/split-lines
       (partition-by string/blank?)
       (filter #(= 2 (count %)))                            ; strip both blank lines _and_ the case found in ENTERPRISE-306 where a submodule was replaced by a dir
       (map (fn [[date-rev file-name]]
              (let [[date rev author] (string/split date-rev #"--")]
                {:date (date-parser date) :rev rev :author author})))))

(defn- to-internal-date-format-string
  [{:keys [date] :as c}]
  (assoc c :date (tf/unparse (:year-month-day tf/formatters) date)))

(defn file-versions-with-author-metadata
  "Returns a seq of maps with the keys :date :rev :author.
   The history tracking respects renames. The results are
   delivered in reverse order (that is, the oldest commit first) sorted
   by their author date."
  [git-cmd repo start-date file-name]
  (let [from-rfc (fn [d] (tf/parse (tf/with-zone (:rfc822 tf/formatters) (t/default-time-zone)) d))]
    (->> (raw-log-with-single-column-result git-cmd repo "%ad--%h--%aN" git-rfc-date-format (as-start-date start-date) "--follow" "--stat" "--name-only" "--" file-name)
         (as-authored-commits from-rfc)
         (sort-by :date)
         (map to-internal-date-format-string))))

(defn historic-file-versions-metadata
  "Returns a seq of maps with the keys :date :rev :name.
   The history tracking respects renames. The results are
   delivered in reverse order (that is, the oldest commit first) sorted
   by their author date."
  [git-cmd repo start-date file-name]
  (let [from-rfc (fn [d] (tf/parse (tf/with-zone (:rfc822 tf/formatters) (t/default-time-zone)) d))]
    (->> (raw-log-with-single-column-result git-cmd repo "%ad--%h" git-rfc-date-format (as-start-date start-date) "--follow" "--stat" "--name-only" "--" file-name)
         (as-commits from-rfc)
         (sort-by :date)
         (map to-internal-date-format-string))))

(defn historic-file-versions-with-metadata
  "Returns a seq of maps with the keys :date :content.
   The history tracking respects renames. The results are
   delivered in reverse order (that is, the oldest commit first) sorted
   by their author date."
  [git-cmd repo start-date file-name]
  (->> (historic-file-versions-metadata git-cmd repo start-date file-name)
       (map (partial historic-file-version-of git-cmd repo))))

;; Diffs
;;

(defn diff-revisions
  [git-cmd repo old-rev later-rev file-to-diff]
  (let [[return-status output _] (run-failable-git-cmd
                                   git-cmd "diff" old-rev later-rev "--" file-to-diff :dir repo)]
    (if (zero? return-status)
      {:success true :diff-output output}
      {:success false :diff-output ""})))

(s/def ::success boolean?)
(s/def ::diff-output string?)
(s/def ::diff-result (s/keys :req-un [::success ::diff-output]))

(s/fdef diff-revisions
        :args (s/cat :git-cmd ::mining-specs/git-exe-command
                     :repo ::mining-specs/repository-path
                     :old-rev ::mining-specs/rev
                     :later-rev ::mining-specs/rev
                     :file-to-diff ::mining-specs/name)
        :ret ::diff-result)

;; Commit info
;;
(s/fdef date-of-commit
        :args (s/cat :git-cmd ::mining-specs/git-exe-command
                     :repo ::mining-specs/repository-path
                     :commit-hash ::mining-specs/git-commit-hash)
        :ret (s/or
               :success (s/keys :req-un [::success ::date])
               :failure (s/keys :req-un [::success ::git-error-message])))

(defn date-of-commit
  "Returns the author date of the given commit as 2018-12-17."
  [git-cmd repo commit-hash]
  (let [[return-status output error-message] (run-failable-git-cmd
                                   git-cmd "show"
                                   "-s"
                                   "--date=short"
                                   "--format=%ad"
                                   commit-hash
                                   :dir repo)]
    (if (zero? return-status)
      {:success true :date (clojure.string/trim-newline output)}
      {:success false :git-error-message error-message})))

;; Branches
;;

(defn current-branch
  "Returns the current branch name of `repo-path`."
  [git-cmd repo]
  (let [[return-status output _] (run-failable-git-cmd
                                   git-cmd "branch" :dir repo)]
    (if (zero? return-status)
      (->>
        output
        (string/split-lines)
        (map #(re-find #"^\*\s+(.*)$" %))
        (filter identity)
        first
        second)
      "")))

(s/fdef current-branch
        :args (s/and (s/cat
                       :git-cmd ::mining-specs/git-exe-command
                       :repo ::mining-specs/repository-path)))

(defn branches->seq
  [output]
  (->> output
       string/split-lines
       (map string/triml)))

(defn- remove-symbolic-links
  [branch-specs]
  (remove #(re-find #"\s->\s" %) branch-specs))

(defn branches-by-recency
  "Returns a seq of each branch name sorted by the most recent
   development activity."
  [git-cmd repo]
  (let [[return-status output _] (run-failable-git-cmd
                                   git-cmd "branch" "-a" "--sort=-committerdate" :dir repo)]
    (if (zero? return-status)
      (->> output
           branches->seq
           (remove #(string/starts-with? % "*"))            ; current branch
           remove-symbolic-links
           (map #(string/replace-first % "remotes/origin/" ""))
           distinct)
      [])))

(defn checkout-branch
  [git-cmd repo branch]
  (let [[return-status output err] (run-failable-git-cmd
                                     git-cmd "checkout" branch :dir repo)]
    (if (zero? return-status)
      [true output]
      [false err])))
