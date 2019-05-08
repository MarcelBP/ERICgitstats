(ns codescene.analysis.architecture.logical-level-analyses
  (:require [codescene.analysis.closed-maat-proxy :as maat]
            [evolutionary-metrics.trends.lead-times :as lead-times]
            [codescene.analysis.paths :as paths]
            [codescene.analysis.specs :as specs]
            [codescene.analysis.social-team-analyses :as social-analyses]
            [clojure.spec.alpha :as s]
            [semantic-csv.core :as sc]
            [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [codescene.analysis.knowledge-visualizer :as knowledge-visualizer]))

(defn- commit-group-strategy-from
  [{:keys [temporal-coupling-strategy ticketidpattern]}]
  {:pre [(s/valid? ::specs/temporal-coupling-strategy temporal-coupling-strategy)]}
  (condp = temporal-coupling-strategy
    :by-time {:temporal-by-author-and-day 1}
    :by-ticket-id {:temporal-by-ticket-id      1
                   :message-extraction-pattern ticketidpattern}
    (throw (ex-info (str "Cannot create commit grouping strategy from: " temporal-coupling-strategy) {}))))

(defn- analysis-params-across-commits
  [{:keys [code-maat-params]}
   {:keys [temporal_coupling_across_minrevs
           temporal_coupling_across_minsharedrevs
           temporal_coupling_across_mincoupling
           temporal_coupling_across_maxcoupling
           temporal_coupling_across_maxchangesetsize] :as project-spec}]
  (merge code-maat-params
         (commit-group-strategy-from project-spec)
         {:min-revs temporal_coupling_across_minrevs
          :min-shared-revs temporal_coupling_across_minsharedrevs
          :min-coupling temporal_coupling_across_mincoupling
          :max-coupling temporal_coupling_across_maxcoupling
          :max-changeset-size temporal_coupling_across_maxchangesetsize}))

(defn- analysis-params-for-logical-components
  [{:keys [code-maat-params]}
   {:keys [temporal_coupling_archlevel_across_minrevs
           temporal_coupling_across_maxchangesetsize] :as project-spec}]
  (merge code-maat-params
         (commit-group-strategy-from project-spec)
         {:min-revs temporal_coupling_archlevel_across_minrevs
          :min-shared-revs temporal_coupling_archlevel_across_minrevs
          :min-coupling   5 ; use a low threshold and let the user sort it out in the dynamic filtering view
          :max-coupling 100
          :max-changeset-size temporal_coupling_across_maxchangesetsize}))

(defn temporal-coupling-across-commits
  [{:keys [filtered-log analysis-path-fn] :as context} project-spec dest-file-name]
  (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))
        analysis-params (analysis-params-across-commits context project-spec)]
    (maat/run-closed-maat filtered-log "coupling" destination analysis-params)))

(defn- coupling-pair-in-same-repository?
  [{:keys [entity coupled]}]
  (= (file-patterns/first-segment entity)
     (file-patterns/first-segment coupled)))

(defn temporal-coupling-between-repositories
  [{:keys [analysis-path-fn] :as _context} _project-spec dest-file-name]
  (let [destination (analysis-path-fn dest-file-name)
        coupling-source (analysis-path-fn paths/architectural-grouped-temporal-coupling-csv)
        candidates (->> coupling-source sc/slurp-csv (remove coupling-pair-in-same-repository?))]
    (sc/spit-csv destination candidates)))

(defn temporal-coupling-across-commits-on-logical-components
  [{:keys [architectural-log analysis-path-fn] :as context} project-spec dest-file-name]
  (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))
        analysis-params (analysis-params-for-logical-components context project-spec)]
    (maat/run-closed-maat architectural-log "coupling" destination analysis-params)))

(defn- run-arch-maat
  [{:keys [architectural-log analysis-path-fn code-maat-params]} analysis dest-file-name]
  (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))]
    (maat/run-closed-maat architectural-log analysis destination code-maat-params)))

(defn soc-on-logical-components
  [context _project dest-file-name]
  (run-arch-maat context "soc" dest-file-name))

(defn revisions-on-logical-components
  [context _project dest-file-name]
  (run-arch-maat context "revisions" dest-file-name))

(defn code-age-on-logical-components
  [context _project dest-file-name]
  (run-arch-maat context "age" dest-file-name))

(defn code-churn-on-logical-components
  [context _project dest-file-name]
  (run-arch-maat context "entity-churn" dest-file-name))

(defn main-dev-on-logical-components
  "The main developer on architectural level needs to consider the whole history of the
   project independent of the current analysis time span. That's why we use a different
   evolutionary log here than ther rest of the analyses in this module."
  [{:keys [architectural-knowledge-log analysis-path-fn code-maat-params]} _project dest-file-name]
  (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))]
    (maat/run-closed-maat architectural-knowledge-log "main-dev" destination  code-maat-params)))

(defn- architectural-team-analysis
  [{:keys [architectural-team-knowledge-log analysis-path-fn code-maat-params]} analysis dest-file-name]
  (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))
        team-spec-path (analysis-path-fn paths/author-colors-csv)
        team-params (merge code-maat-params {:team-map team-spec-path
                                             :unknown-team-name "_unassigned_"})]
    (maat/run-closed-maat architectural-team-knowledge-log analysis destination team-params)
    destination))

(defn team-ownership
  [context _project dest-file-name]
  (architectural-team-analysis context "main-dev" dest-file-name))

(defn team-fragmentation
  [context _project dest-file-name]
  (architectural-team-analysis context "fragmentation" dest-file-name))

(defn team-level-entity-ownership ; used to calculate fractals
  [context _project dest-file-name]
  (architectural-team-analysis context "entity-ownership" dest-file-name))

(defn entity-ownership-on-logical-components
  [{:keys [architectural-knowledge-log analysis-path-fn code-maat-params]} _project dest-file-name]
  (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))]
    (maat/run-closed-maat architectural-knowledge-log "entity-ownership" destination code-maat-params)))

(defn temporal-coupling-on-logical-components
  "Calculates temporal coupling on components changed in the same commit."
  [context _project dest-file-name]
  (run-arch-maat context "coupling" dest-file-name))

(defn- analyse-churn-by-ticket
  [{:keys [evo-log analysis-path-fn code-maat-params]} ticketidpattern dest-file-name]
  (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))
        analysis-params (merge code-maat-params {:temporal-by-ticket-id      1
                                                 :message-extraction-pattern ticketidpattern})]
    (maat/run-closed-maat evo-log "abs-churn-by-ticket-id" destination analysis-params)
    destination))

(defn- lead-time-by-date
  [ticket-churn-result-file]
  (->> (shared/read-csv-sans-header-from ticket-churn-result-file)
       (map (fn [row] {:ticket-date (first row) :lead-time (Integer/parseInt (last row))})))) ; NOTE: watch out - we depend upon position here!

(defn code-churn-by-ticket-id
  [{:keys [analysis-path-fn] :as context} {:keys [ticketidpattern] :as _project-spec} dest-file-name]
  (when (and (some? ticketidpattern)
             (not (empty? ticketidpattern)))
    (let [ticket-churn-result-file (analyse-churn-by-ticket context ticketidpattern dest-file-name)
          lead-time-trends-result-file (analysis-path-fn paths/ticket-lead-time-trend)
          lead-times (lead-time-by-date ticket-churn-result-file)]
          (lead-times/trend-by-month-to-file lead-times lead-time-trends-result-file))))

(defn knowledge-loss-analysis
  [{:keys [architectural-knowledge-log architectural-complexity-log] :as context} project dest-file-name]
  (let [loss-input {:social-log architectural-knowledge-log
                    :complexity-log architectural-complexity-log}
        maat-params {:unknown-team-name knowledge-visualizer/active-developers-team}]
    (social-analyses/analyze-knowledge-loss project loss-input context paths/lost-authors-csv "main-dev" maat-params paths/raw-arch-knowledge-loss-csv dest-file-name)))


