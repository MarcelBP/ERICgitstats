(ns codescene.analysis.paths
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as log]))

;; Development Output (Brooks's Law)
(def ^:const development-output-json "development-output.json")

;; Input data

(def ^:const technical-vcs-log "filtered.id")

;; System level analysis

(def ^:const system-level-trend-csv "systemleveltrend.csv")
(def ^:const stacked-architectural-complexity-csv "stackedarchcomplexity.csv")
(def ^:const stacked-architectural-loc-csv "stackedarchloc.csv")

(def ^:const authors-per-month-trend "authors_per_month_trend.csv")

(def ^:const file-summary-csv "filesummary.csv")
(def ^:const summary-csv "summary.csv")

(def ^:const system-map-json "systemmap.json")

(def ^:const hotspots-json "new_hotspots.json")
(def ^:const hotspots-csv  "new_hotspots.csv")
(def ^:const hotspots-by-churn-csv "hotspots_by_churn.csv")

(def ^:const augmented-hotspots-csv "augmented_hotspots.csv")
(def ^:const revisions-csv "revisions.csv")

(def ^:const trend-revisions-csv "historicrevisions.csv")

(def ^:const classified-spots-csv "classified_spots.csv")
(def ^:const classified-spot-stats-csv "classified_spot_stats.csv")

(def ^:const abs-churn-csv "abs-churn.csv")
(def ^:const loc-trend-csv "loctrend.csv")
(def ^:const churn-trend-csv "churntrend.csv")
(def ^:const revision-trend-csv "revision-churn.csv")

(def ^:const abs-churn-by-ticket-id "abs_churn_by_ticket_id.csv")
(def ^:const ticket-lead-time-trend "ticket_lead_time_trend.csv")

(def ^:const temporal-edge-json "temporal_edge.json")
(def ^:const temporal-csv "coupling.csv")
(def ^:const temporal-soc-csv "soc.csv")

(def ^:const social-main-dev "main-dev.csv")

(def ^:const warning-hotspots-csv "hotspot_deltas.csv")

(def ^:const early-warnings-json "early_warnings.json")

(def ^:const communication-edge-json "communication_bundle.json")

(def ^:const knowledge-enclosure-json "knowledge_enclosure.json")
(def ^:const raw-knowledge-loss-csv "rawknowledgeloss.csv")
(def ^:const knowledge-loss "knowledge_loss")
(def ^:const knowledge-loss-csv "knowledge_loss.csv")
(def ^:const knowledge-loss-json "knowledge_loss.json")
(def ^:const all-authors-csv "all-authors.csv")
(def ^:const lost-authors-csv "lost_authors.csv")
(def ^:const lost-authors-team-color-csv "lost_authors_color.csv")
(def ^:const lost-authors-team-color "lost_authors_color")
(def ^:const author-colors "author_colors") ; Since we have a (stupid?) convention in the visualizers...
(def ^:const author-colors-csv (str author-colors ".csv"))
(def ^:const author-churn-csv "author-churn.csv")

(def ^:const team-knowledge "knowledge_teams")
(def ^:const team-knowledge-csv "knowledge_teams.csv")
(def ^:const knowledge-teams-json "knowledge_teams.json")
(def ^:const team-colors "team_colors")
(def ^:const team-colors-csv "team_colors.csv")
(def ^:const team-level-author-churn-csv "team-level-author-churn.csv")
(def ^:const team-level-author-timeline-csv "team-level-author-timeline.csv")

(def ^:const detected-ex-devs-warning-csv "lost_authors_detected.csv")

(def ^:const social-system-map-file-name "socialsystemmap.json")
(def ^:const social-team-system-map-file-name "socialteamsystemmap.json")
(def ^:const social-architectural-system-map-file-name "socialarchitectualsystemmap.json")

(def ^:const architectural-fragmentation-csv-file-name "archlevelfragmentation.csv")

(def ^:const fragmentation-csv-file-name "fragmentation.csv")
(def ^:const fragmentation-with-complexity-csv-file-name "fragmentationcomplexity.csv")
(def ^:const fragmentation-json-file-name "fragmentation.json")
(def ^:const fractal-figures-csv-file-name "fractals.csv")
(def ^:const entity-ownership-csv-file-name "entity-ownership.csv")

(def ^:const entity-ownership-team-csv-file-name "entityownersteam.csv")
(def ^:const fragmentation-team-csv-file-name "fragmentationteam.csv")
(def ^:const fragmentation-team-with-complexity-csv-file-name "fragmentationteamcomplexity.csv")
(def ^:const fragmentation-team-json-file-name "fragmentationteam.json")
(def ^:const fractal-figures-team-csv-file-name "fractalsteam.csv")

(def ^:const age-csv-file-name "age.csv")
(def ^:const age-json-file-name "age.json")
(def ^:const age-trend-csv-file-name "age_trend.csv")

(def ^:const architectural-grouped-temporal-coupling-csv "grupedtemporal.csv")
(def ^:const architectural-grouped-temporal-edge-json "groupedtemporal.json")

(def ^:const temporal-coupling-between-repos-csv "temporal_coupling_between_repos.csv")
(def ^:const temporal-coupling-between-repos-json "temporal_coupling_between_repos.json")

;;
;; Project Management Metrics
;;

(def ^:const pm-data-description-csv "pm_data_description.csv")
(def ^:const pm-resolved-log "pm_evo_data.csv")
(def ^:const pm-item-input-data-csv "costs.csv")
(def ^:const pm-cost-by-entity-csv "pm_cost_by_entity.csv")
(def ^:const pm-cost-by-logical-component-csv "pm_cost_by_logcomp.csv")
(def ^:const pm-cost-hotspots-by-logical-component-csv "pm_costhotspots_by_logcomp.csv")
(def ^:const pm-cost-hotspots-csv "pm_cost_hotspots.csv")
(def ^:const pm-cost-hotspots-json "pm_cost_hotspots.json")
(def ^:const pm-cost-architectural-hotspots-json "pm_cost_by_logcomp.json")

(def ^:const costs-trend-path-name "coststrends")
(def ^:const costs-trend-file-name "costtrend.csv")
(def ^:const work-trend-file-name "worktrend.csv")

(def ^:const architectural-costs-trend-path-name "archcoststrends")
(def ^:const pm-arch-resolved-log "pm_archevo_data.csv")

;;
;; Modus operandi
;;

(def ^:const modus-commit-message-trend-csv "modus_commit_message_trend.csv")

; We can generate trends for parts of the modus commit pattern too. For example,
; the pattern bug|defect results in two sub-patterns, "bug" and "defect".
; The result files are based on a common root path to which we append a hash of the pattern name.
(def ^:const modus-commit-message-partial-path "modus_commit_message_partial")
; The following document will contain the mapping between pattern and result file:
(def ^:const modus-commit-message-sub-trends-csv "modus_commit_message_sub_trends.csv")

;;
;; Defect density
;;
(def ^:const defects-by-file-csv "defects_by_file.csv")
(def ^:const defect-density-by-file-csv "defect_density_by_file.csv")
(def ^:const defect-trends-by-file-csv "defect_trends_by_file.csv")

;;
;; Risk classification
;;

(def ^:const diffusion-by-revision-csv "diffusion-by-revision.csv")
(def ^:const risk-by-commit-csv "risk-by-commit.csv")
(def ^:const rolling-risk-csv "rolling-risk.csv")
(def ^:const risk-by-commit-early-warnings-csv "risk-by-commit-early-warnings-csv")

;;
;; Architectural level analyses on logical and user-defined components:
;;

(def ^:const architectural-level-commit-coupling-csv "archlevelsinglecoupling.csv")
(def ^:const architectural-level-commit-coupling-json "archlevelsinglecoupling.json")
(def ^:const architectural-level-coupling-csv "archlevelcoupling.csv")
(def ^:const architectural-level-coupling-json "archlevelcoupling.json")
(def ^:const architectural-level-soc-csv "archlevelsoc.csv")
(def ^:const architectural-level-revisions-csv "archlevelrevisions.csv")
(def ^:const architectural-level-code-age-csv "archlevelcodeage.csv")
(def ^:const architectural-level-hotspots-csv "archlevelhotspots.csv")
(def ^:const architectural-level-hotspots-json "archlevelhotspots.json")
(def ^:const architectural-level-raw-code-churn-csv "archlevelrawchurn.csv")
(def ^:const architectural-level-code-churn-csv "archlevelchurn.csv")
(def ^:const architectural-level-system-map-json "archlevelsystemmap.json")
(def ^:const architectural-level-main-dev-csv "archlevelmaindev.csv")
(def ^:const architectural-level-ownership-csv "archlevelownership.csv")
(def ^:const architectural-level-fractals-csv "archlevelfractals.csv")
(def ^:const architectural-level-team-fractals-csv "archlevelteamfractals.csv")

(def ^:const architectural-level-team-ownership-csv "archlevelteamdev.csv")
(def ^:const architectural-level-all-teams-ownership-csv "archlevelallteamsownership.csv")

(def ^:const architectural-technical-sprawl-csv "archleveltechsprawl.csv")

(def ^:const raw-arch-knowledge-loss-csv "archrawknowledgeloss.csv")
(def ^:const arch-knowledge-loss-csv "archknowledgeloss.csv")

(def ^:const all-time-revisions-csv "all_time_revisions.csv")

(def ^:const code-bio-marker-scores-csv "code_markers.csv")
(def ^:const code-bio-marker-details-json "code_markers_detail.json")

(def ^:const dashboard-csv "dashboard.csv")

;; Branches and CI/CD
;;
(def ^:const branch-statistics-csv "branch_statistics.csv")

;; Note watcher
;;
(def ^:const risks-for-notes-json "risks-for-notes.json")
(def ^:const lost-notes-json "lost-notes.json")

;; Aggregated architectural scores
;;
(def ^:const system-hotspots-health-csv "system_hotspots_health.csv")

(defn as-child-path
  [parent child]
  (.getPath (io/file parent child)))

(defn has-extension?
  [n]
  (re-find #"\.[^.]*$" n))

(defn analysis->result-path
  [analysis]
  (if (has-extension? analysis)
    analysis
    (str analysis ".csv")))

(defn as-indexed-file-name
  [parent base-name index extension]
  (as-child-path parent (str base-name index "." extension)))

(defn file-seq-matching
  [directory-name file-base-name]
  (filter #(.startsWith (.getName %) file-base-name)
          (file-seq (io/file directory-name))))

(defn make-analysis-path-to
  [parent analysis-name]
  (.getPath (io/file parent analysis-name)))

(defn spot-index->trend-file-name
  [i]
  (str "trend" i ".csv"))

(defn concat-file-content
  "Concatenates the content of all files with the given base name into a new
   file designated by the destination-file-name.
   This function also takes an optional argument that lets you filter the
   lines in each file before concatenation. The typical usage is to skip a
   possible header in sub-sequent concatenations."
  ([dir base-name destination-file-name]
   (concat-file-content dir base-name destination-file-name identity))
  ([dir base-name destination-file-name line-seq-offset-fn]
   (let [files (file-seq-matching dir base-name)
         use-first-file-for-header (first files)

         ;; prevent concatenating destination-file-name onto itself if it already is present
         ;; (risk of infinite loop/recursion here)
         remaining-files (remove (fn [f] (= (.getPath f) destination-file-name)) (rest files))]
     (try
       (io/copy (io/file use-first-file-for-header) (io/file destination-file-name))
       (with-open [w (clojure.java.io/writer destination-file-name :append true)]
         (doseq [f remaining-files]
           (with-open [rdr (io/reader f)]
             (doseq [line (line-seq-offset-fn (line-seq rdr))] ; skip the header
               (.write w line)
               (.newLine w)))))
       destination-file-name
       (catch IllegalArgumentException e
         (do
           (log/error (str "Failed to combine mined data from " dir ", " base-name " using the headers from " use-first-file-for-header) e)
           (throw e)))))))

(defn concat-csv-file-content
  [dir csv-base-name destination-file-name]
  (concat-file-content dir csv-base-name destination-file-name next))
