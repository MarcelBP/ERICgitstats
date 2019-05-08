(ns codescene.analysis.specs
  (:require [clojure.spec.alpha :as s]
            [evolutionary-metrics.analysis.coupling-algos]
            [evolutionary-metrics.core]
            [evolutionary-metrics.trends.specs]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as string]
            [clj-time.coerce :as tc]
            [clj-time.format :as tf])
  (:import (org.joda.time DateTime)
           (org.joda.time.base BaseSingleFieldPeriod)))

;;; generic

(s/def ::filename string?)
(s/def ::datetime (s/with-gen #(instance? DateTime %)
                              #(gen/fmap tc/from-date (s/gen (s/inst-in #inst "2005" #inst "2016-09")))))
(s/def ::date-str (s/with-gen (s/and string? #(re-matches #"\d{4}-\d{2}-\d{2}" %))
                              #(gen/fmap (fn [dt] (tf/unparse (:year-month-day tf/formatters) dt)) (s/gen ::datetime))))

;;; run-analysis

(s/def ::temporal-coupling-strategy #{:by-time :by-ticket-id})

(s/def ::file-extensions-pattern (s/and string?
                                        (complement string/blank?)
                                        #(re-matches #"^\*\..+$" %)))
(s/def ::empty-string (s/and string? empty?))

(s/def ::glob-pattern string?)

(s/def ::commit-hash (s/and string? #(re-matches #"[0-9a-f]+" %))) ; note: we don't want to be too precise since the (short) hash varies with Git version.
(s/def ::exclude-commits (s/coll-of ::commit-hash))

(comment
  {:keys [repo-paths analysis-destination ncomplexitytrends] :as project}
  {:keys [warning-reporter-fn
          authors
          teams
          architectural-transformations
          git-client
          scramble?
          time-now] :as analysis-request})

(s/def ::analysis-version nat-int?)
(s/def ::repo-paths (s/coll-of ::filename))
(s/def ::analysis-destination ::filename)
(s/def ::ncomplexitytrends nat-int?)
(s/def ::complexitytrendwarninglookbackinmonths nat-int?)
(s/def ::exclusionfilter (s/nilable (s/or :provided ::file-extensions-pattern
                                          :empty-exclusions ::empty-string)))
(s/def ::excludecontent (s/nilable ::glob-pattern))
(s/def ::whitelistcontent (s/nilable ::glob-pattern))
(s/def ::analysis-start-date ::date-str)
(s/def ::team-analysis-start-date ::date-str)
(s/def ::age-is-last-commit boolean?)
(s/def ::spottrendspan nat-int?)

(s/def ::time-period #(instance? BaseSingleFieldPeriod %)) ; e.g. (tc/months 3)
(s/def ::cut-off-age-for-active-author ::time-period)

(s/def ::minrevs :evolutionary-metrics.analysis.coupling-algos/minrevs)
(s/def ::minsharedrevs :evolutionary-metrics.analysis.coupling-algos/minsharedrevs)
(s/def ::mincoupling :evolutionary-metrics.analysis.coupling-algos/mincoupling)
(s/def ::maxcoupling :evolutionary-metrics.analysis.coupling-algos/maxcoupling)
(s/def ::maxchangesetsize :evolutionary-metrics.analysis.coupling-algos/maxchangesetsize)

(s/def ::temporal_coupling_archlevel_across_minrevs :evolutionary-metrics.analysis.coupling-algos/minrevs)

(s/def ::rollingaveragedays :evolutionary-metrics.trends.specs/rolling-avg-days)
(s/def ::thresholdinsdyellow :evolutionary-metrics.trends.specs/threshold-in-sd-yellow)
(s/def ::thresholdinsdred :evolutionary-metrics.trends.specs/threshold-in-sd-red)
(s/def ::percentage (s/int-in 0 100))
(s/def ::minpercentageyellow ::percentage)
(s/def ::minpercentagered ::percentage)
(s/def ::trendperiodinmonths :evolutionary-metrics.trends.specs/trend-period-in-months)
(s/def ::minloc :evolutionary-metrics.trends.specs/min-loc)
(s/def ::minsamplepoints :evolutionary-metrics.trends.specs/min-sample-points)
(s/def ::social-net-min-shared-revs nat-int?)
(s/def ::update-repositories boolean?)
(s/def ::update-subcommand (s/nilable #{:pull :fetch}))
(s/def ::include-deleted-content boolean?)
(s/def ::complete-history-for-social boolean?)

;; Look for specific messages or words in the commit clouds:
(s/def ::modus-commit-message-pattern (s/or :not-present nil? :present (partial re-pattern)))
;; If enabled, we split the modues commit message pattern ("|") and calculate separate
;; sub-graphs:
(s/def ::split-modus-commit-graphs boolean?)

(s/def ::filter-repository-content-on-git-ls boolean?) ; true for on premise, false for cloud

(s/def ::hotspot-defect-commit-pattern ::modus-commit-message-pattern)

(s/def ::pair-programming-pattern (s/or :not-present nil? :present (partial re-pattern)))

(s/def ::mine-git-in-parallel boolean?)

(s/def ::lookup-copied-content boolean?)

(s/def ::handle-nonascii-paths? boolean?)

(s/def ::system-level-trends #{:no-system-level-trend
                               :run-system-level-trend
                               :run-system-level-trend-when-suitable})

(s/def ::minfilesize  nat-int?)
(s/def ::minrelativerevs ::percentage)

(s/def ::auto-detect-text-files boolean?)

(s/def ::git-rename-limit nat-int?) ; how deep should Git's diff.renameLimit go

;; Support an alias map that translate developer names as specified by
;; the user in the UI.
(s/def ::developer-git-name string?)
(s/def ::developer-resolved-alias string?)

(s/def ::developer-alias-map (s/map-of ::developer-git-name ::developer-resolved-alias))

;; Options to calculate CI/CD statistics based on branch usage
;;
(s/def ::calculate-branch-statistics boolean?)
(s/def ::branches-analysis-lookback-months nat-int?)
(s/def ::predict-branch-delivery-risk boolean?)
(s/def ::branch-delivery-risk-commit-limit nat-int?)
(s/def ::prune-removed-remote-branches boolean?)

;; Code Biomarkers
(s/def ::calculate-code-biomarkers boolean?)

;; Aggregate Code Biomarkers for architectural components.
;; We use this field as a feature toggle.
(s/def ::calculate-system-level-biomarkers boolean?)

;; The project is the main model for an analysis

(s/def ::project (s/keys :req-un [::repo-paths
                                  ::analysis-destination
                                  ::complexitytrendwarninglookbackinmonths
                                  ::exclusionfilter
                                  ::excludecontent
                                  ::minfilesize
                                  ::minrelativerevs
                                  ::whitelistcontent
                                  ::analysis-start-date
                                  ::team-analysis-start-date
                                  ::spottrendspan
                                  ::auto-detect-text-files
                                  ::minrevs
                                  ::minsharedrevs
                                  ::mincoupling
                                  ::maxcoupling
                                  ::maxchangesetsize
                                  ::temporal_coupling_archlevel_across_minrevs
                                  ::rollingaveragedays
                                  ::filter-repository-content-on-git-ls
                                  ::update-repositories
                                  ::include-deleted-content
                                  ::complete-history-for-social
                                  ::modus-commit-message-pattern
                                  ::hotspot-defect-commit-pattern
                                  ::pair-programming-pattern
                                  ::social-net-min-shared-revs
                                  ::system-level-trends
                                  ::cut-off-age-for-active-author
                                  ::lookup-copied-content
                                  ::developer-alias-map
                                  ::age-is-last-commit]
                         :opt-un [::split-modus-commit-graphs
                                  ::mine-git-in-parallel
                                  ::handle-nonascii-paths?
                                  ::exclude-commits
                                  ::update-subcommand  ; TODO: move to required commands later
                                  ::calculate-branch-statistics
                                  ::branches-analysis-lookback-months
                                  ::predict-branch-delivery-risk
                                  ::branch-delivery-risk-commit-limit
                                  ::prune-removed-remote-branches
                                  ::calculate-code-biomarkers
                                  ::git-rename-limit
                                  ::calculate-system-level-biomarkers]))

(s/def ::warning-reporter-fn fn?)
(s/def ::id any?)
(s/def ::name string?)
(s/def ::color (s/nilable string?))
(s/def ::exdev any?)
(s/def ::author (s/keys :req-un [::name]
                        :opt-un [::id
                                 ::exdev]))
(s/def ::authors (s/coll-of ::author))
(s/def ::team-map (s/keys :req-un [::id
                                   ::name]))
(s/def ::teams (s/coll-of ::team-map))
(s/def ::pattern ::glob-pattern)
(s/def ::transformation string?)
(s/def ::architectural-transformation (s/keys :req-un [::pattern ::transformation]))
(s/def ::architectural-transformations (s/coll-of ::architectural-transformation))
(s/def ::git-client string?)
(s/def ::scramble? boolean?)
(s/def ::time-now ::datetime)

(s/def ::analysis-params (s/keys :req-un [::warning-reporter-fn
                                          ::authors
                                          ::teams
                                          ::architectural-transformations
                                          ::git-client
                                          ::scramble?
                                          ::time-now]))

(s/def ::start-time ::datetime)
(s/def ::end-time ::datetime)
(s/def ::duration nat-int?)
(s/def ::path ::filename)
(s/def ::git-head-revision string?)                         ;TODO: maybe more specific, eg regex
(s/def ::git-head-revisions (s/coll-of ::git-head-revision))

;; TODO: Move this section to analysis-warnings
;; begin
(comment

  {::warning-type ::rising-hotspots
   ::hotspots     []}

  {::warning-type   ::complexity-increase
   ::yellow-markers 2
   ::red-markers    1}

  {::warning-type     ::ex-authors
   ::authors          [{:name "Kalle"}]
   ::threshold-months 6}

  {::warning-type      ::high-risk-commits
   ::number-of-risk-commits 7}

  )
(s/def :codescene.analysis.analysis-warnings/warning-type keyword?)
(s/def :codescene.analysis.analysis-warnings/hotspots
  (s/coll-of (s/cat :name string?
                    :revisions int?
                    :old-rank int?
                    :new-rank int?
                    :rank int?)))
(s/def :codescene.analysis.analysis-warnings/yellow-markers nat-int?)
(s/def :codescene.analysis.analysis-warnings/red-markers nat-int?)
(s/def :codescene.analysis.analysis-warnings/authors (s/coll-of (s/cat :author-name string?
                                                                       :last-contribution string?)))
(s/def :codescene.analysis.analysis-warnings/threshold-months nat-int?)
(s/def :codescene.analysis.analysis-warnings/supervision-warnings nat-int?)

(s/def :codescene.analysis.analysis-warnings/author-increase number?)
(s/def :codescene.analysis.analysis-warnings/monthly-output-decrease number?)
(s/def :codescene.analysis.analysis-warnings/weekly-authors-delta number?)
(s/def :codescene.analysis.analysis-warnings/weekly-output-decrease number?)

;; multi-spec for warning-type
(defmulti warning-type :codescene.analysis.analysis-warnings/warning-type)
(defmethod warning-type :codescene.analysis.analysis-warnings/rising-hotspots [_]
  (s/keys :req [:codescene.analysis.analysis-warnings/hotspots]))
(defmethod warning-type :codescene.analysis.analysis-warnings/complexity-increase [_]
  (s/keys :req [:codescene.analysis.analysis-warnings/yellow-markers
                :codescene.analysis.analysis-warnings/red-markers]))
(defmethod warning-type :codescene.analysis.analysis-warnings/ex-authors [_]
  (s/keys :req [:codescene.analysis.analysis-warnings/authors
                :codescene.analysis.analysis-warnings/threshold-months]))
(defmethod warning-type :codescene.analysis.analysis-warnings/high-risk-commits [_]
  (s/keys :req [:codescene.analysis.analysis-warnings/number-of-risk-commits]))
(defmethod warning-type :codescene.analysis.analysis-warnings/branch-long-ttl [_]
  (s/keys :req [:codescene.analysis.analysis-warnings/red-warnings
                :codescene.analysis.analysis-warnings/yellow-warnings]))
(defmethod warning-type :codescene.analysis.analysis-warnings/notes-category-warning [_]
  (s/keys :req [:codescene.analysis.analysis-warnings/supervision-warnings]))

(defmethod warning-type :codescene.analysis.analysis-warnings/monthly-brooks-law-category-warning [_]
  (s/keys :req [:codescene.analysis.analysis-warnings/author-increase
                :codescene.analysis.analysis-warnings/monthly-output-decrease]))
(defmethod warning-type :codescene.analysis.analysis-warnings/monthly-development-output-category-warning [_]
  (s/keys :req [:codescene.analysis.analysis-warnings/monthly-output-decrease]))
(defmethod warning-type :codescene.analysis.analysis-warnings/weekly-onboarding-output-category-warning [_]
  (s/keys :req [:codescene.analysis.analysis-warnings/weekly-authors-delta
                :codescene.analysis.analysis-warnings/weekly-output-decrease]))

(s/def ::analysis-warning (s/multi-spec warning-type :codescene.analysis.analysis-warnings/warning-type))
(s/def ::analysis-warnings (s/coll-of ::analysis-warning))
;; end

(s/def ::n-complexity-trends nat-int?)

(comment
  {:path               "/Users/ulrik/Source/Git/empear/Mvc/results/Mvc/analysis201609201407",
   :name               "analysis201609201407",
   :time-now           "2016-09-20T14:07:18.759Z",          ; org.joda.time.DateTime
   :start-time         "2016-09-20T14:07:58.817Z",          ; org.joda.time.DateTime
   :end-time           "2016-09-20T14:08:35.866Z",          ; org.joda.time.DateTime
   :duration           37,
   :git-head-revisions '("01f7ecd9d9fc90b42a3d80c089b6e69d3916d127"),
   :analysis-warnings  '({:codescene.analysis.analysis-warnings/warning-type   :codescene.analysis.analysis-warnings/complexity-increase,
                          :codescene.analysis.analysis-warnings/yellow-markers 1,
                          :codescene.analysis.analysis-warnings/red-markers    0}),
   :authors            '({:name "Ajay Bhargav Baaskaran"}
                          {:name "Anthony van der Hoorn"}
                          {:name "Ben Adams"}
                          {:name "Brennan"})
   :n-complexity-trends 100})

(s/def ::analysis-result (s/keys :req-un [::start-time
                                          ::end-time
                                          ::time-now
                                          ::duration
                                          ::path
                                          ::name
                                          ::git-head-revisions
                                          ::authors
                                          ::n-complexity-trends]))

;;; mine-logs

(comment

  ;; project-metrics
  {}                                                        ;; or
  {:pm-costs-log        cost-file-name
   :pm-type-of-work-log type-of-work-file-name}

  ;; analysis-logs
  {:scrambled-translation        (clojure.set/map-invert scrambled-names)
   :evo-log                      evo-log
   :filtered-log                 technical-log
   :individual-socal-log         individual-log
   :social-log                   social-log
   :trend-log                    trend-log
   :complexity-log               complexity-log
   :architectural-log            architectural-log
   :architectural-complexity-log architectural-complexity-log}

  )
(s/def ::scrambled-translation map?)                        ;TODO: need to be specified
(s/def ::evo-log ::filename)
(s/def ::filtered-log ::filename)
(s/def ::social-log ::filename)
(s/def ::trend-log ::filename)
(s/def ::complexity-log ::filename)
(s/def ::architectural-complexity-log ::filename)

(s/def ::analysis-path ::filename)
(s/def ::analysis-path-fn fn?)
(s/def ::architectural-log ::filename)

(s/def ::mine-project-metrics-result (s/keys :opt-un [::pm-costs-log
                                                      ::pm-type-of-work-log]))
(s/def ::mine-analysis-logs-result (s/keys :req-un [::scrambled-translation
                                                    ::evo-log
                                                    ::filtered-log
                                                    ::social-log
                                                    ::trend-log
                                                    ::complexity-log
                                                    ::architectural-log
                                                    ::architectural-complexity-log]))
(s/def ::mine-logs-result (s/merge ::mine-project-metrics-result
                                   ::mine-analysis-logs-result))

;;; make-analysis-context!

(comment
  {:authors-to-exclude            (names-of-excluded authors)
   :architectural-transformations architectural-transformations
   :git-client                    git-client
   :scramble-enabled              scramble?})

(s/def ::authors-to-exclude (s/and set? (s/coll-of string?)))
(s/def ::scramble-enabled ::scramble?)

(s/def ::analysis-context-params (s/keys :req-un [::authors-to-exclude
                                                  ::architectural-transformations
                                                  ::git-client
                                                  ::scramble-enabled]))

(comment
  {;; analysis-logs
   :scrambled-translation         {},
   :evo-log                       "/efs/analysis/results/Mvc/analysis201609160924/completeevolution.id",
   :filtered-log                  "/efs/analysis/results/Mvc/analysis201609160924/filtered.id",
   :individual-socal-log          "/efs/analysis/results/Mvc/analysis201609160924/individualsociallog.id",
   :social-log                    "/efs/analysis/results/Mvc/analysis201609160924/filteredsocial.id",
   :trend-log                     "/efs/analysis/results/Mvc/analysis201609160924/historiclog.id",
   :complexity-log                "/efs/analysis/results/Mvc/analysis201609160924/complexity.csv",
   :architectural-log             "/efs/analysis/results/Mvc/analysis201609160924/architectural.id",
   :architectural-complexity-log  "/efs/analysis/results/Mvc/analysis201609160924/archcomplexity.csv",
   ;; project-metrics
   :pm-costs-log                  ""                        ;optional
   :pm-type-of-work-log           ""                        ;optional
   ;; analysis-context-params
   :authors-to-exclude            #{},
   :architectural-transformations [],
   :git-client                    "git",
   :scramble-enabled              false,
   ;; the rest
   :analysis-path                 "/efs/analysis/results/Mvc/analysis201609160924",
   :analysis-start-date           "2015-08-20",
   :analysis-path-fn              nil,
   :code-maat-params              {:min-revs           10,
                                   :min-shared-revs    10,
                                   :min-coupling       30,
                                   :max-coupling       100,
                                   :max-changeset-size 50,
                                   :age-time-now       "2016-09-16"}}
  )

;; TODO: change names in code-maat to be dashed, so we can use them directly as keys
(s/def ::min-revs :evolutionary-metrics.analysis.coupling-algos/minrevs)
(s/def ::min-shared-revs :evolutionary-metrics.analysis.coupling-algos/minsharedrevs)
(s/def ::min-coupling :evolutionary-metrics.analysis.coupling-algos/mincoupling)
(s/def ::max-coupling :evolutionary-metrics.analysis.coupling-algos/maxcoupling)
(s/def ::max-changeset-size :evolutionary-metrics.analysis.coupling-algos/maxchangesetsize)
(s/def ::age-time-now ::date-str)                           ;TODO: move to code-maat

(s/def ::code-maat-params (s/keys :req-un [::min-revs
                                           ::min-shared-revs
                                           ::min-coupling
                                           ::max-coupling
                                           ::max-changeset-size
                                           ::age-time-now]))

(s/def ::analysis-context-result (s/keys :req-un [::analysis-path
                                                  ::analysis-path-fn
                                                  ::analysis-start-date
                                                  ::code-maat-params]))

(s/def ::analysis-context (s/merge ::mine-logs-result
                                   ::analysis-context-params
                                   ::analysis-context-result))
