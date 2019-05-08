(ns codescene.analysis.analysis-task
  (:require [clj-time.core :as tc]
            [clj-time.format :as f]
            [clojure.spec.alpha :as s]
            [codescene.analysis.analysis-task-impl :as details]
            [codescene.analysis.architecture.logical-level-analyses :as logical-level-analyses]
            [codescene.analysis.colors :as colors]
            [codescene.analysis.dashboard-generator :as dashboard]
            [codescene.analysis.author-discovery :as developer-discovery]
            [codescene.analysis.diagnostics :as diagnostics]
            [codescene.analysis.analysis-warnings :as analysis-warnings]
            [codescene.analysis.fractal-figures :as fractals]
            [codescene.analysis.hotspot-complexity-trends :as complexity-trends]
            [codescene.analysis.knowledge-visualizer :as knowledge-visualizer]
            [codescene.analysis.modus-operandi :as modus-operandi]
            [codescene.analysis.paths :as paths]
            [codescene.analysis.project-management-metrics :as pm-metrics]
            [codescene.analysis.social-team-analyses :as social-team-analyses]
            [codescene.velocity.development-output :as development-output]
            [codescene.analysis.versioning :as versioning]
            [codescene.risks.prediction :as prediction]
            [codescene.analysis.architecture.aggregated-trends :as aggregated-trends]
            [codescene.analysis.specs :as specs]
            [codescene.analysis.author-metrics :as author-metrics]
            [codescene.analysis.defect-density :as defect-density]
            [codescene.analysis.visualization.file-level-hotspots :as file-level-enclosure-visualizer]
            [codescene.analysis.visualization.architectural-hotspots :as architectural-level-enclosure-visualizer]
            [codescene.analysis.visualization.cost-hotspots :as cost-enclosure-visualizer]
            [codescene.analysis.visualization.social-organization :as social-visualizer]
            [codescene.analysis.visualization.architectural-social-organization :as architectural-social-visualizer]
            [codescene.analysis.architecture.technical-sprawl :as technical-sprawl]
            [codescene.branches.branch-statistics :as branch-statistics]
            [codescene.biomarkers.file-level-hotspots :as file-level-bio-markers]
            [codescene.biomarkers.architectural-level-hotspots :as arch-level-biomarkers]
            [codescene.validations.repos :as vrepos]
            [evolutionary-metrics.mergers.hotspots :as hotspots]
            [taoensso.timbre :as timbre]
            [codescene.thresholds.analysis-thresholds :as analysis-thresholds]
            [clojure.java.io :as io]
            [codescene.note-watch.supervised-scores :as notes-scores]
            [evolutionary-metrics.mining.vcs :as vcs]))

;;; We define a vector of all analyses we want to run:
(def ^:private analyses-to-run
  ["abs-churn" "revision-churn" "entity-churn"])

(def ^:private author-analyses
  ["summary" "all-authors" "author-churn" "author-timeline"])

;; Some analyses shall be run on filtered input data:
(def ^:private technical-analyses ["revisions" "coupling" "soc" "age" "diffusion-by-revision"])

(def ^:private social-analyses ["main-dev" "fragmentation" "entity-ownership"])

(def ^:private analyses-on-logical-level
  [{:runner-fn logical-level-analyses/temporal-coupling-across-commits :result paths/architectural-grouped-temporal-coupling-csv}
   {:runner-fn logical-level-analyses/temporal-coupling-across-commits-on-logical-components :result paths/architectural-level-coupling-csv}
   {:runner-fn logical-level-analyses/temporal-coupling-between-repositories :result paths/temporal-coupling-between-repos-csv}
   {:runner-fn logical-level-analyses/soc-on-logical-components :result paths/architectural-level-soc-csv}
   {:runner-fn logical-level-analyses/revisions-on-logical-components :result paths/architectural-level-revisions-csv}
   {:runner-fn logical-level-analyses/code-age-on-logical-components :result paths/architectural-level-code-age-csv}
   {:runner-fn logical-level-analyses/main-dev-on-logical-components :result paths/architectural-level-main-dev-csv}
   {:runner-fn logical-level-analyses/team-ownership :result paths/architectural-level-team-ownership-csv}
   {:runner-fn logical-level-analyses/team-fragmentation :result paths/architectural-fragmentation-csv-file-name}
   {:runner-fn logical-level-analyses/entity-ownership-on-logical-components :result paths/architectural-level-ownership-csv}
   {:runner-fn logical-level-analyses/team-level-entity-ownership :result paths/architectural-level-all-teams-ownership-csv}
   {:runner-fn logical-level-analyses/knowledge-loss-analysis :result paths/arch-knowledge-loss-csv}
   {:runner-fn logical-level-analyses/code-churn-on-logical-components :result paths/architectural-level-raw-code-churn-csv}
   {:runner-fn logical-level-analyses/temporal-coupling-on-logical-components :result paths/architectural-level-commit-coupling-csv}
   {:runner-fn logical-level-analyses/code-churn-by-ticket-id :result paths/abs-churn-by-ticket-id}])

(def ^:private project-management-analyses
  [{:runner-fn pm-metrics/analyze-costs-of :result paths/pm-cost-by-entity-csv}
   {:runner-fn pm-metrics/analyze-architectural-costs-of :result paths/pm-cost-by-logical-component-csv}])

(def ^:private modus-operandi-analyses
  [{:runner-fn modus-operandi/analyze-commit-message-trend :result paths/modus-commit-message-trend-csv}
   {:runner-fn defect-density/defects-by-file :result paths/defects-by-file-csv}])

(defn- aggregating-analyses-to-run
  "Returns a sequence of aggregated analyses. These analyses depend on one or more raw analysis
   results. A typical example includes a visualizer the generates an artifact that we feed to
   the client side views."
  [project context]
  [{:results   [paths/defect-density-by-file-csv]
    :runner-fn defect-density/density-by-max-defects
    :input     [paths/defects-by-file-csv "revisions"]}
   {:results   [paths/defect-trends-by-file-csv]
    :runner-fn (partial defect-density/defect-trends-by-hotspot context project)
    :input     [paths/defects-by-file-csv]}

   {:results   [paths/pm-cost-hotspots-csv]
    :runner-fn hotspots/hotspots-by-complexity-and-revisions
    :input     ["complexity" "pm_cost_by_entity"]}
   {:results   [paths/pm-cost-hotspots-json]
    :runner-fn (partial cost-enclosure-visualizer/write-enclosure-graph project)
    :input     [paths/pm-cost-hotspots-csv "main-dev"]}

   {:results   [paths/pm-cost-hotspots-by-logical-component-csv]
    :runner-fn hotspots/hotspots-by-complexity-and-revisions
    :input     ["archcomplexity" paths/pm-cost-by-logical-component-csv]}
   {:results   [paths/pm-cost-architectural-hotspots-json]
    :runner-fn (partial cost-enclosure-visualizer/write-enclosure-graph project)
    :input     [paths/pm-cost-hotspots-by-logical-component-csv paths/architectural-level-main-dev-csv]}

   {:results   [paths/hotspots-csv]
    :runner-fn hotspots/hotspots-by-complexity-and-revisions
    :input     ["complexity" "revisions"]}
   {:results   [paths/hotspots-by-churn-csv]
    :runner-fn hotspots/hotspots-by-relative-code-churn
    :input     ["complexity" "entity-churn"]}

   {:results   [paths/architectural-technical-sprawl-csv]
    :runner-fn (partial technical-sprawl/by-main-language-of-component-to-disk context)
    :input     [paths/hotspots-csv]}

   {:results   [paths/architectural-level-hotspots-csv]
    :runner-fn hotspots/hotspots-by-complexity-and-revisions
    :input     ["archcomplexity" "archlevelrevisions"]}
   {:results   [paths/architectural-level-code-churn-csv]
    :runner-fn hotspots/hotspots-by-relative-code-churn
    :input     ["archcomplexity" paths/architectural-level-raw-code-churn-csv]}

   {:results   [paths/architectural-level-system-map-json]
    :runner-fn (partial architectural-level-enclosure-visualizer/write-enclosure-graph project)
    :input     [paths/architectural-level-hotspots-csv
                paths/architectural-level-main-dev-csv
                paths/architectural-level-code-age-csv
                paths/arch-knowledge-loss-csv
                paths/architectural-level-code-churn-csv
                paths/architectural-technical-sprawl-csv]}

   {:results   [paths/social-architectural-system-map-file-name]
    :runner-fn (partial architectural-social-visualizer/write-enclosure-graph project)
    :input     [paths/architectural-level-hotspots-csv
                paths/architectural-level-main-dev-csv
                paths/architectural-level-team-ownership-csv
                paths/arch-knowledge-loss-csv
                paths/architectural-fragmentation-csv-file-name
                paths/architectural-technical-sprawl-csv]}

   {:results   [paths/social-system-map-file-name]
    :runner-fn (partial social-visualizer/write-enclosure-graph project)
    :input     [paths/hotspots-csv
                "main-dev"
                paths/knowledge-loss
                paths/fragmentation-csv-file-name]}

   {:results   [paths/social-team-system-map-file-name]
    :runner-fn (partial social-visualizer/write-enclosure-graph project)
    :input     [paths/hotspots-csv
                paths/team-knowledge
                paths/knowledge-loss ; metrics for individuals, but it's the same on a team level
                paths/fragmentation-team-csv-file-name]}

   {:results   [paths/churn-trend-csv]
    :runner-fn (partial details/generate-churn-trend project)
    :input     ["abs-churn"]}
   {:results   [paths/loc-trend-csv]
    :runner-fn details/generate-loc-trend
    :input     ["abs-churn" "complexity"]}
   {:results   [paths/temporal-edge-json]
    :runner-fn details/generate-temporal-edge-bundle
    :input     ["coupling"]}
   {:results   [paths/architectural-grouped-temporal-edge-json]
    :runner-fn details/generate-temporal-edge-bundle
    :input     [paths/architectural-grouped-temporal-coupling-csv]}
   {:results   [paths/temporal-coupling-between-repos-json]
    :runner-fn details/generate-temporal-edge-bundle
    :input     [paths/temporal-coupling-between-repos-csv]}
   {:results   [paths/architectural-level-coupling-json]
    :runner-fn details/generate-temporal-edge-bundle
    :input     [paths/architectural-level-coupling-csv]}
   {:results   [paths/architectural-level-commit-coupling-json]
    :runner-fn details/generate-temporal-edge-bundle
    :input     [paths/architectural-level-commit-coupling-csv]}
   {:results   [paths/communication-edge-json]
    :runner-fn details/generate-communication-edge-bundle
    :input     ["communication"]}

   {:results   [paths/age-trend-csv-file-name]
    :runner-fn details/generate-age-trend
    :input     ["complexity" "age"]}

   ; Fractals for files
   {:results   [paths/fractal-figures-csv-file-name]
    :runner-fn fractals/generate-fractal-figures
    :input     [paths/entity-ownership-csv-file-name paths/author-colors]}
   {:results   [paths/fractal-figures-team-csv-file-name]
    :runner-fn fractals/generate-team-level-fractal-figures
    :input     [paths/fragmentation-team-csv-file-name paths/entity-ownership-team-csv-file-name paths/team-colors]}

   ; Fractals for architectural components
   {:results   [paths/architectural-level-fractals-csv]
    :runner-fn fractals/generate-fractal-figures
    :input     [paths/architectural-level-ownership-csv paths/author-colors]}
   {:results   [paths/architectural-level-team-fractals-csv]
    :runner-fn fractals/generate-fractal-figures
    :input     [paths/architectural-level-all-teams-ownership-csv paths/team-colors]}

   ])

(defn- analyses-to-run-on-complete-data ; run these once everything else has been run...
  [project context]
  ; Code biomarkers
  [{:results   [paths/code-bio-marker-scores-csv paths/code-bio-marker-details-json]
    :runner-fn (partial file-level-bio-markers/write-scores project context)
    :input     [paths/classified-spots-csv paths/hotspots-csv paths/risks-for-notes-json]}

   ; Architectural level biomarkers
   {:results   [paths/system-hotspots-health-csv]
    :runner-fn (partial arch-level-biomarkers/aggregate-code-health-scores project context)
    :input     [paths/architectural-level-hotspots-csv
                "architectural.id" ; for defect calculations
                paths/architectural-level-team-ownership-csv
                paths/architectural-fragmentation-csv-file-name
                paths/arch-knowledge-loss-csv
                paths/risks-for-notes-json
                paths/risk-by-commit-csv]}

   ; System map on file level
   {:results   [paths/system-map-json]
   :runner-fn (partial file-level-enclosure-visualizer/write-enclosure-graph project)
   :input     [paths/hotspots-csv
               paths/social-main-dev
               "age"
               paths/knowledge-loss
               paths/hotspots-by-churn-csv
               paths/classified-spots-csv
               paths/code-bio-marker-scores-csv
               paths/defect-density-by-file-csv]}
   ])

(def ^:private knowledge-color-spec
  ; knowledge loss:
  {:knowledge-loss-color               colors/color-for-knowledge-loss
   :active-developer-color             colors/active-developers-team-color
   :lost-authors-output-file-name      paths/lost-authors-csv
   :lost-authors-team-output-file-name paths/lost-authors-team-color-csv
   ; Single author knowledge map colors:
   :author-colors-output-file-name     paths/author-colors-csv
   ; Team knowledge map colors:
   :team-colors-output-file-name       paths/team-colors-csv})

(defn- run-analyses-in-group
  [context project analyses]
  (doseq [{:keys [runner-fn result]} analyses]
    (runner-fn context project result)))

(def ^:private time-stamp-formatter (f/formatter "yyyyMMddHHmmss"))

(defn- analysis-name-from
  [time-stamp]
  (str "analysis"
       (f/unparse time-stamp-formatter time-stamp)))

(defn- names-of-excluded
  "Returns the names of excluded authors in the given seq."
  [authors]
  (->> authors
       (filter :excluded)
       (map :name)
       (into #{})))

(defn- current-git-head-revisions
  [git-client repo-paths]
  (map (partial vcs/git-head-revision-in git-client) repo-paths))

(defn- branch-statistics-for
  [{:keys [calculate-branch-statistics] :as project} {:keys [analysis-path-fn] :as context}]
  (timbre/info "Calculate branch statistics: " calculate-branch-statistics)
  (if calculate-branch-statistics
    (branch-statistics/branch-statistics-to-disk context
                                                 project
                                                 (analysis-path-fn paths/branch-statistics-csv))
    []))

(s/fdef run-analysis
        :args (s/cat :project ::specs/project
                     :analysis-params ::specs/analysis-params)
        :ret ::specs/analysis-result)


;;; NOTE: This looks like a great opportunity to parallelize some analyses, but we need
 ;;; to take care: the main bottleneck is memory, not speed. It's probably better to run
;;; the analyses sequentially and optimize each step by shrinking the analysis data by means
;;; of pre-filtering. Has proven successful so far...
;;;
;;; We also HAVE TO refactor this - it has become a mess, mainly due to
;;; the inflexible signature of the visualizers (pass in some more context
;;; and we'll be fine).
(defn run-analysis
  [{:keys [repo-paths analysis-destination] :as project}
   {:keys [warning-reporter-fn
           authors
           teams
           architectural-transformations
           git-client
           scramble?
           time-now
           notes]}]
  (vrepos/assert-analysis-constrains-fullfiled repo-paths)
  (let [start-time (tc/now)
        analysis-time-now (or time-now start-time)
        analysis-name (analysis-name-from analysis-time-now)
        analysis-path (paths/make-analysis-path-to analysis-destination analysis-name)
        analysis-version (versioning/write-analysis-version analysis-path)
        analysis-params {:authors-to-exclude            (names-of-excluded authors)
                         :architectural-transformations architectural-transformations
                         :git-client                    git-client
                         :scramble-enabled              scramble?}
        full-context (details/make-analysis-context! analysis-path project warning-reporter-fn analysis-params analysis-time-now analysis-version)
        context (dissoc full-context :scrambled-translation) ; we don't want to expose the translations in the debug traces

        ;; Here we start running analyses
        _ (timbre/infof "Running analyses and visualizers in '%s'." analysis-path)

        ;; Tricky dependencies ahead: we've just run an all-authors analysis to see if there are any new authors that
        ;; weren't known earlier. Now we need that information to continue with the social analyses.
        ;; TODO: make dependencies like this explicit in the design.
        _ (doseq [analysis analyses-to-run]
            (details/analyze-on-complete-evolution context analysis))

        _ (doseq [a author-analyses]
            (details/analyze-author-statistics context a))

        ;; These depend on "all-authors" analysis being run before:
        all-author-names (apply sorted-set (developer-discovery/all-authors-from context project))
        updated-author-names (developer-discovery/merge-detected-authors-with-configured all-author-names authors)
        notes-risks (notes-scores/analyze-risks-for-notes! project context analysis-path (or notes []))]

    (knowledge-visualizer/generate-knowledge-colors! context knowledge-color-spec updated-author-names teams)
    (doseq [analysis technical-analyses]
      (details/analyze-technical context analysis))
    (doseq [analysis social-analyses]
      (details/analyze-social project context analysis))
    (social-team-analyses/run-analyses context project)
    (run-analyses-in-group context project analyses-on-logical-level)
    (run-analyses-in-group context project project-management-analyses)
    (run-analyses-in-group context project modus-operandi-analyses)
    (author-metrics/contributors-per-month context project paths/authors-per-month-trend)
    (doseq [visualization (aggregating-analyses-to-run project context)]
      (details/visualize context visualization))
    (let [hotspot-stats (analysis-thresholds/describe-hotspots ((:analysis-path-fn context) paths/hotspots-csv))
          {:keys [n-complexity-trends] :as complexity-warning-thresholds} (analysis-thresholds/complexity-trend-warning-thresholds hotspot-stats)
          information-for-system-trends? (complexity-trends/generate-hotspot-trends-from full-context project n-complexity-trends)
          branches (branch-statistics-for project context)]
      (timbre/trace "Detect Complexity Trend Warnings with thresholds: " (pr-str complexity-warning-thresholds))
      (aggregated-trends/system-level-complexity-trend context
                                                       project
                                                       information-for-system-trends?
                                                       architectural-transformations
                                                       paths/system-level-trend-csv)
      (complexity-trends/augment-hotspots-with-trend-warnings project context complexity-warning-thresholds)
      (pm-metrics/generate-costs-trends context project complexity-warning-thresholds)
      (prediction/risk-by-commit context
                                 project
                                 {:risk-dest-file-name paths/risk-by-commit-csv
                                  :rolling-risk-dest-file-name paths/rolling-risk-csv})
      (development-output/velocity-delta-from-file-results context paths/development-output-json)
      (analysis-warnings/generate-early-warnings context project branches)
      (details/classify-hotspots project context notes-risks)
      (doseq [visualization (analyses-to-run-on-complete-data project context)] ; so, all analyses are run - fire-off the final visualizations
        (details/visualize context visualization))
      (dashboard/write-analysis-highlights context project updated-author-names teams branches)
      (let [end-time (tc/now)
            ;; realize the lazy seq before returning it, thus avoiding timing problems for caller
            git-head-revisions (doall (current-git-head-revisions git-client repo-paths))]
        {:start-time         start-time
         :end-time           end-time
         :time-now           analysis-time-now
         :duration           (tc/in-seconds (tc/interval start-time end-time))
         :path               analysis-path
         :name               analysis-name
         :git-head-revisions git-head-revisions
         :authors            updated-author-names
         :n-complexity-trends n-complexity-trends
         :notes-risks        notes-risks}))))

;; Setup a timbre debug trace of each function we invoke in the analysis.
;; This is our way of tracing down potential problems during development:
(def ^:private namespaces-to-trace
  ['codescene.analysis.social-team-analyses 'codescene.analysis.analysis-task-impl
   'codescene.analysis.knowledge-visualizer 'codescene.analysis.dashboard-generator
   'codescene.analysis.architecture.logical-level-analyses
   'codescene.analysis.architecture.aggregated-trends])

(defn- start-trace-analysis-progress
  []
  (doseq [ns-to-trace namespaces-to-trace]
    (diagnostics/trace-ns ns-to-trace)))

(defn- stop-trace-analysis-progress
  []
  (doseq [ns-to-trace namespaces-to-trace]
    (diagnostics/untrace-ns ns-to-trace)))

(defn init
  []
  (start-trace-analysis-progress))

(defn shutdown
  []
  (stop-trace-analysis-progress))
