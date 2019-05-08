(ns codescene.branches.branch-statistics
  (:require [evolutionary-metrics.mining.branch-activity :as branch-activity]
            [evolutionary-metrics.trends.dates :as dates]
            [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [codescene.stats.math :as stats]
            [clj-time.core :as tc]
            [semantic-csv.core :as sc]
            [taoensso.timbre :as log]
            [evolutionary-metrics.mining.vcs :as vcs]
            [codescene.delta.delta-analysis :as delta-analysis]
            [codescene.delta.risk-classification :as risk-classification]
            [clojure.spec.alpha :as s]
            [codescene.analysis.specs :as analysis-specs]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [codescene.analysis.paths :as paths]
            [codescene.risks.predictor-mining :as predictors]))

(def ^:const red-ttl-warning "red")
(def ^:const yellow-ttl-warning "yellow")
(def ^:const no-warning "none")

(defn- hours-between
  [start-date stop-date]
  (max (tc/in-hours (tc/interval (dates/date-time-string->date start-date)
                                 (dates/date-time-string->date stop-date)))
       1))

(defn- highest-risk-for-commit-range
  [author-experience-lookup diffusions project git-client repo commit-hashes]
  (let [parsed-commits (->> commit-hashes (delta-analysis/as-evolutionary-log project git-client repo) (group-by :rev) vals)]
    (reduce (fn [highest-risk commit]
              (let [{:keys [risk-value]} (risk-classification/commit-risk-with-known-predictors author-experience-lookup diffusions commit)]
                (if (>= risk-value 10)
                  (reduced 10)
                  (max risk-value highest-risk))))
            1
            parsed-commits)))

(def ^:private indeterminated-risk {:hasriskprediction 0 :risk 0})

(defn- branch-exceeds-risk-threshold?
  "Some organizations use long lived branches that gather hundreds of commits.
   Calculating risk here may take some time, so make it optional."
  [{:keys [branch-delivery-risk-commit-limit] :as _project} {:keys [n-commits] :as _branch}]
  (if branch-delivery-risk-commit-limit
       (<= branch-delivery-risk-commit-limit n-commits)
       false))

(defn- predict-risk-on
  [{:keys [git-client analysis-path-fn] :as _context}
   {:keys [predict-branch-delivery-risk branch-delivery-risk-commit-limit] :as project}
   repo
   merged?
   {:keys [first-commit last-commit n-commits] :as branch}]
  (log/debug "Check risk prediction on " (:name branch) " with " n-commits
             " commits, prediction = " predict-branch-delivery-risk
             " , threshold: " branch-delivery-risk-commit-limit)
  (if (and predict-branch-delivery-risk
           (not merged?)
           (not (branch-exceeds-risk-threshold? project branch)))
    (try
      (log/info "Predict risk on the branch " (:name branch) " with " n-commits " commits.")
      (let [commits (vcs/commits-in-range git-client repo first-commit last-commit)]
        (if (every? seq commits)
          (let [author-stats-file (analysis-path-fn paths/author-churn-csv)
                author-experience-lookup (->> author-stats-file shared/read-csv-sans-header-from predictors/author-experience-lookup)
                diffusion-file (analysis-path-fn paths/diffusion-by-revision-csv)
                diffusions (->> diffusion-file shared/read-csv-sans-header-from (risk-classification/diffusion-for-repository repo))
                risk (highest-risk-for-commit-range author-experience-lookup diffusions project git-client repo commits)]
            {:hasriskprediction 1
             :risk risk})
          indeterminated-risk))
      (catch Throwable e
        (do
          (log/warn "Failed to calculate risk for " (:name branch) ": " e)
          indeterminated-risk)))
    (do
      (log/debug "Skip risk prediction of branch " (:name branch) " with " n-commits " commits.")
      indeterminated-risk)))

(defn- statistics-on
  [context project repo {:keys [name last-date first-date merge-date n-commits n-authors] :as branch}]
  (let [merged? (some? merge-date)
        risk (predict-risk-on context project repo merged? branch)]
    (merge risk
           {:name name
            :start first-date
            :end last-date
            :merged merged?
            :commits n-commits
            :authors n-authors
            :ttl (hours-between first-date last-date)
            :leadmerge (if (some? merge-date) (hours-between last-date merge-date) 0)})))

(defn- reference-branch ; TODO: let the user configure this!
  [git-client repo]
  (let [current (vcs/current-branch git-client repo)]
    (if (empty? current)
      "master"
      current)))

(defn- correct-branch-span?
  [{:keys [first-date last-date]}]
  (let [start (dates/date-time-string->date first-date)
        end (dates/date-time-string->date last-date)]
    (or (tc/equal? start end) ; a single commit on a branch
        (tc/after? end start))))

(defn- add-ttl-warning-status
  [time-now red-point yellow-point {:keys [ttl end merged] :as b}]
  ; no warning unless there's recent work (last week - a heuristic) on the branch.
  (let [cutoff-date-for-warning (tc/minus (dates/string->date time-now) (tc/weeks 1))
        recent-work? (tc/after? (dates/date-time-string->date end) cutoff-date-for-warning)]
    (assoc b :ttlwarning
             (cond
               (or merged (false? recent-work?)) no-warning
               (> ttl red-point) red-ttl-warning
               (> ttl yellow-point) yellow-ttl-warning
               :else no-warning))))

(defn- outlier?
  [upper lower v]
  (or (> v upper)
      (< v lower)))

(defn- outlier-aware-cut-off-points-for
  [vs]
  (let [descriptive-all (stats/stats-for vs)
        upper-outlier-point (+ (:mean descriptive-all) (* 2 (:sd descriptive-all)))
        lower-outlier-point (max 1 (- (:mean descriptive-all) (* 2 (:sd descriptive-all))))
        {:keys [sd mean]} (some->> vs (remove (partial outlier? upper-outlier-point lower-outlier-point)) stats/stats-for)]
    {:yellow-warning-point (+ mean (* 2 sd))
     :red-warning-point (+ mean (* 3 sd))}))

(defn- detect-ttl-warnings
  [time-now bs-stats]
  (let [all-ttls (map :ttl bs-stats)
        {:keys [yellow-warning-point red-warning-point]} (outlier-aware-cut-off-points-for all-ttls)]
    (map (partial add-ttl-warning-status time-now red-warning-point yellow-warning-point) bs-stats)))

(defn- detect-author-diffusion-warnings
  [bs-stats]
  (let [authors-by-branch (map :authors bs-stats)
        {:keys [yellow-warning-point red-warning-point]} (outlier-aware-cut-off-points-for authors-by-branch)]
    (map (fn [{:keys [authors] :as b}]
           (assoc b :authorwarning
                    (cond
                      (> authors red-warning-point) red-ttl-warning
                      (> authors yellow-warning-point) yellow-ttl-warning
                      :else no-warning)))
         bs-stats)))

(s/def ::hours nat-int?)

(s/def ::warning-types #{red-ttl-warning yellow-ttl-warning no-warning})
(s/def ::ttlwarning ::warning-types)
(s/def ::authorwarning ::warning-types)

(s/def ::hasriskprediction (s/and nat-int? #{0 1}))
(s/def ::risk (s/and nat-int? (set (range 11))))

(s/def ::merged boolean?)
(s/def ::repository string?) ; it's just the name for presentation purposes
(s/def ::leadmerge ::hours)
(s/def ::name ::branch-activity/name)
(s/def ::start ::branch-activity/first-date)
(s/def ::ttl ::hours)
(s/def ::commits ::branch-activity/n-commits)
(s/def ::end ::branch-activity/last-date)
(s/def ::authors ::branch-activity/n-authors)

;; We do have full specs for both context and project, but if we use them
;; we don't get an easy way to test it via REPL. So let's specify stripped
;; down versions containing just the keys of interest in this context.
(s/def ::analysis-context (s/keys :req-un
                                  [::analysis-specs/git-client
                                   ::analysis-specs/age-time-now]))
(s/def ::project (s/keys :req-un
                         [::analysis-specs/developer-alias-map
                          ::analysis-specs/repo-paths]
                         :req-opt
                         [::analysis-specs/branches-analysis-lookback-months
                          ::analysis-specs/branch-delivery-risk-commit-limit]))

(s/def ::branch-statistic (s/keys :req-un
                                  [::ttlwarning
                                   ::authorwarning
                                   ::merged
                                   ::repository
                                   ::leadmerge
                                   ::name
                                   ::start
                                   ::ttl
                                   ::commits
                                   ::end
                                   ::authors
                                   ::hasriskprediction
                                   ::risk]))

(s/def ::branch-statistics (s/coll-of ::branch-statistic))

(defn overview
  [{:keys [git-client age-time-now] :as context} {:keys [developer-alias-map branches-analysis-lookback-months] :as project} repo]
  (log/debug "Calculate branch statistics for the repository: " repo)
  (try
    (let [main-branch (reference-branch git-client repo)
          branch-mining-context {:branches-analysis-lookback-months branches-analysis-lookback-months
                                 :time-now age-time-now
                                 :developer-alias-map developer-alias-map}
          bs (branch-activity/development-statistics git-client repo branch-mining-context main-branch)
          branches-with-data (filter (comp some? :first-date) bs)
          branches-missing-data (remove (comp some? :first-date) bs)
          valid-branches (filter correct-branch-span? branches-with-data)
          invalid-branches (remove correct-branch-span? branches-with-data)
          repo-name (file-patterns/final-segment repo)]
      (log/info "Inspecting a total of " (count bs) " recent branches since " age-time-now ". Reference branch: " main-branch)
      (doseq [{:keys [name first-commit last-commit]} branches-missing-data]
        (log/warn "Missing branch data for: " name + " [" first-commit " , " last-commit "]"))
      (doseq [{:keys [name first-commit last-commit]} invalid-branches]
        (log/warn "Skip branch " name " since its commit range couldn't be calculated: [" first-commit " , " last-commit "]"))
      (->> valid-branches
           (map (partial statistics-on context project repo))
           (map #(assoc % :repository repo-name))
           (detect-ttl-warnings age-time-now)
           detect-author-diffusion-warnings))
    (catch Throwable e
      (do
        (log/warn "Failed to calculate statistics for the branches in repo = " repo e)
        []))))

(s/fdef overview
        :args (s/cat :_context ::analysis-context
                     :_project ::project
                     :repo ::repository)
        :ret ::branch-statistics)

(defn branch-statistics-to-disk
  [context {:keys [repo-paths] :as project} output-file-name]
  (try
    (let [branches-by-repo (pmap (partial overview context project) repo-paths)
          sorted-branches (->> branches-by-repo (apply concat) (sort-by :end (comp - compare)))]
      (sc/spit-csv output-file-name sorted-branches)
      sorted-branches)
    (catch Throwable e
      (log/error "Failed to calculate branch statistics" e) ; just swallow it since we want the rest of the analysis to run
      [])))

(s/fdef branch-statistics-to-disk
        :args (s/cat :context ::analysis-context
                     :project ::project
                     :output-file-name ::analysis-specs/filename)
        :ret ::branch-statistics)
