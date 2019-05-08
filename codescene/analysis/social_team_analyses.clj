(ns codescene.analysis.social-team-analyses
  (:require [codescene.analysis.closed-maat-proxy :as maat]
            [codescene.analysis.pair-programming-configurer :as pp-conf]
            [codescene.analysis.paths :as paths]
            [codescene.analysis.knowledge-visualizer :as knowledge-visualizer]
            [evolutionary-metrics.mergers.knowledge-adjuster :as knowledge-adjuster]))

(defn- analyze-teams
  [{:keys [team-analysis-log analysis-path-fn code-maat-params]} {:keys [social-net-min-shared-revs] :as _project} filename analysis]
  (let [destination (analysis-path-fn (paths/analysis->result-path analysis))
        team-spec-path (analysis-path-fn filename)
        team-params (merge code-maat-params {:known-authors team-spec-path
                                             :augment-author-teams 1
                                             :social-net-min-shared-revs social-net-min-shared-revs})]
    (maat/run-closed-maat team-analysis-log analysis destination team-params)))

(defn- analyze-team-level
  [context-dependent-evo-log {:keys [analysis-path-fn code-maat-params]} team-spec analysis optional-analysis-parameters dest-file-name]
  (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))
        team-spec-path (analysis-path-fn team-spec)
        team-params (merge code-maat-params {:team-map team-spec-path} optional-analysis-parameters)]
    (maat/run-closed-maat context-dependent-evo-log analysis destination team-params)
    destination))

(defn analyze-knowledge-loss
  [project {:keys [social-log complexity-log]} {:keys [analysis-path-fn] :as context} team-spec analysis maat-params raw-results-file-name dest-file-name]
  (let [raw-results (analyze-team-level social-log context team-spec analysis maat-params raw-results-file-name)
        destination (analysis-path-fn (paths/analysis->result-path dest-file-name))]
    (knowledge-adjuster/adjust-ownership-by-code-size complexity-log raw-results destination)))

(defn run-analyses
  [{:keys [team-analysis-log individual-socal-log complexity-log] :as context} project]
  (analyze-teams context project paths/author-colors-csv "communication")
  (analyze-team-level team-analysis-log context paths/author-colors-csv "main-dev" {} paths/team-knowledge-csv)
  (analyze-team-level team-analysis-log context paths/author-colors-csv "fragmentation" {} paths/fragmentation-team-csv-file-name)
  (analyze-team-level team-analysis-log context paths/author-colors-csv "entity-ownership" {} paths/entity-ownership-team-csv-file-name)
  (analyze-team-level team-analysis-log context paths/author-colors-csv "author-churn" {} paths/team-level-author-churn-csv)
  (analyze-team-level team-analysis-log context paths/author-colors-csv "author-timeline" {} paths/team-level-author-timeline-csv)
  (let [loss-input {:social-log individual-socal-log
                    :complexity-log complexity-log}]
    (analyze-knowledge-loss project loss-input context paths/lost-authors-csv "main-dev" {:unknown-team-name knowledge-visualizer/active-developers-team} paths/raw-knowledge-loss-csv paths/knowledge-loss-csv)))
