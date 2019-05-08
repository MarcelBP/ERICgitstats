(ns evolutionary-metrics.mining.branch-activity
  (:require [evolutionary-metrics.mining.vcs :as vcs]
            [taoensso.timbre :as log]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.mining.specs :as mining-specs]
            [evolutionary-metrics.core :as core-specs]
            [clojure.string :as string]
            [clj-time.core :as tc]
            [evolutionary-metrics.trends.dates :as dates]))

(defn- most-recent-commit-on-branches
  "Delivers a seq with branch info sorted on the branches with most
   recent commit activity."
  [git-cmd repo]
  (let [[return-status output _err] (vcs/run-failable-git-cmd
                                      git-cmd "branch" "-r" "--sort=-committerdate"
                                      "--format=%(refname)   %(objectname)   %(authordate:iso-strict)"
                                      :dir repo)]
    (if (zero? return-status)
      (->> output
           vcs/branches->seq
           (map #(string/split % #"\s{3}"))
           (remove #(or (string/includes? % "/master") (string/includes? % "/HEAD")))
           (map (fn [[b c d]] {:name b :last-commit c :last-date d})))
      [])))

(defn- first-commit-stats-from
  [commits]
  (->> commits
       (map (fn [[commit author-date _author]] {:first-commit commit :first-date author-date}))
       first))

(defn- n-distinct-authors-from
  [developer-alias-map commits]
  (->> commits
       (map last)
       (map (fn [author] (get developer-alias-map author author))) ; resolve aliases
       distinct
       count))

(defn- n-distinct-commits-in
  [cs]
  (->> cs (map first) distinct count))

(defn- workflow-aware-branch-point-in
  [last-commit boundary-git-output]
  (let [candidates (->> boundary-git-output
                        vcs/branches->seq
                        (filter #(string/starts-with? % "-"))
                        (map #(string/replace % #"\-" "")))
        candidate (first candidates)]
    (if (= candidate last-commit) ; GitHub workflow, not sure why this happens
      (second candidates)
      candidate)))

(defn- detailed-contextual-error
  [{:keys [name last-commit]} reason]
  (str "Failed to calculate detailed statistics for " name ", reference: " last-commit ", reason: " reason))

(defn- detailed-statistics-for-branch-point
  "In order to handle both merged and unmerged branches, we look for the difference between
   the branch-point (e.g. last commit on master from which the branch was created) and the
   last known commit on the branch itself."
  [git-cmd repo developer-alias-map reference-branch {:keys [name last-commit] :as branch}]
  (let [[return-status output err] (vcs/run-failable-git-cmd
                                     git-cmd "rev-list" "--reverse"
                                     "--boundary" (str name "..." reference-branch)
                                     :dir repo)]
    (if (zero? return-status)
      (if-let [branch-point (workflow-aware-branch-point-in last-commit output)]
        (let [[first-res first-output first-err] (vcs/run-failable-git-cmd
                                                   git-cmd "log" "--reverse"
                                                   (str branch-point ".." last-commit)
                                                   "--date=iso-strict"
                                                   "--no-merges" "--first-parent" ; exclude commits merged from master
                                                   "--pretty=format:%h   %ad   %aN"
                                                   :dir repo)]
          (if (zero? first-res)
            (let [commits (->> first-output vcs/branches->seq (map #(string/split % #"\s{3}")))]
              {:success true
               :first-commit (first-commit-stats-from commits)
               :n-commits (n-distinct-commits-in commits)
               :n-authors (n-distinct-authors-from developer-alias-map commits)})
            {:success false :error (detailed-contextual-error branch first-err)}))
        {:success false :error (detailed-contextual-error branch "No branch point found.")})
      {:success false :error (detailed-contextual-error branch err)})))

(defn- merge-date
  [git-cmd repo reference-branch last-commit-on-branch]
  (let [[return-status output _err] (vcs/run-failable-git-cmd
                                     git-cmd "log"
                                     (str last-commit-on-branch ".." reference-branch)
                                     "--ancestry-path"
                                     "--merges" "--pretty=format:%cd"
                                     "--date=iso-strict" "--reverse"
                                     :dir repo)]
    (when (zero? return-status)
      (->> output vcs/branches->seq first))))

(s/def ::branch string?)

(s/def ::name string?)

(s/def ::developer-alias-map (s/map-of string? string?))

(s/def ::last-commit ::mining-specs/git-commit-hash)
(s/def ::first-commit (s/or ::first-commit ::mining-specs/git-commit-hash
                            ::no-first-commit empty?))

(s/def ::last-date ::core-specs/basic-date-time-string)
(s/def ::first-date (s/nilable ::core-specs/basic-date-time-string))
(s/def ::merge-date (s/nilable ::core-specs/basic-date-time-string))

(s/def ::n-commits nat-int?)
(s/def ::n-authors nat-int?)

(s/def ::branch-activity (s/keys :req-un [::name
                                          ::n-authors
                                          ::n-commits
                                          ::last-commit
                                          ::last-date
                                          ::first-commit
                                          ::first-date
                                          ::merge-date]))

(s/def ::activity-on-branches (s/coll-of ::branch-activity))

(s/def ::context map?)

(defn- recent-branch?
  [cut-off-date {:keys [last-date]}]
  (let [d (dates/date-time-string->date last-date)]
    (tc/after? d cut-off-date)))

(def ^:private default-lookback-months 2)

(defn- branches-of-interest
  [time-now branches-analysis-lookback-months bs]
  (let [cut-off-date (tc/minus (dates/string->date time-now)
                               (tc/months (or branches-analysis-lookback-months default-lookback-months)))]
    (take-while (partial recent-branch? cut-off-date) bs)))

(defn development-statistics
  "Returns a sequence describing the activity on each branch.
   The returned branches are sorted on recency with the branch
   with the last commit delivered first."
  [git-cmd repo {:keys [branches-analysis-lookback-months time-now developer-alias-map] :as context} reference-branch]
  (let [all-branches (most-recent-commit-on-branches git-cmd repo)
        bs (branches-of-interest time-now branches-analysis-lookback-months all-branches)]
    (reduce (fn [acc {:keys [last-commit] :as b}]
              (let [{:keys [success first-commit n-commits n-authors error]} (detailed-statistics-for-branch-point git-cmd repo developer-alias-map reference-branch b)
                    m (merge-date git-cmd repo reference-branch last-commit)]
                (if success
                  (do
                    (log/debug "Branch statistics for " (:name b) " : " last-commit)
                    (conj acc (merge b
                                     first-commit
                                     {:merge-date (when-not (empty? m) m)
                                      :n-commits n-commits
                                      :n-authors n-authors})))
                  (do
                    (log/debug "Skip branch " (:name b) ", reason: " error)
                    acc))))
            []
            bs)))

(s/fdef development-statistics
        :args (s/cat :git-cmd ::mining-specs/git-exe-command
                     :repo ::mining-specs/repository-path
                     :context ::context
                     :reference-branch ::branch)
        :ret ::activity-on-branches)
