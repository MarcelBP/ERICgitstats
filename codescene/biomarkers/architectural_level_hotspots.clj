(ns codescene.biomarkers.architectural-level-hotspots
  (:require [codescene.analysis.paths :as paths]
            [codescene.analysis.architecture.aggregator :as architectural-aggregator]
            [codescene.risks.by-architectural-component :as architectural-risks]
            [codescene.note-watch.utils :as nu]
            [semantic-csv.core :as sc]
            [digest :as digest]
            [clojure.data.json :as json]
            [codescene.biomarkers.code-markers-analysis :as code-markers-analysis]
            [codescene.analysis.defect-density :as defect-density]
            [evolutionary-metrics.app.date-filter :as commit-date-filter]
            [codescene.queries.social-information :as social-information]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [codescene.biomarkers.candidate-selection :as candidate-selection]
            [codescene.analysis.visualization.parse-analysis-results :as parse]
            [evolutionary-metrics.trends.dates :as dates]
            [clj-time.core :as tc]))

(def ^:private architectural-biomarkers-folder "archbiomarkers")

(defn score-path
  [analysis-result-directory component-name]
  (->> (io/file (str (digest/sha-1 component-name) ".csv"))
       (io/file architectural-biomarkers-folder)
       (io/file analysis-result-directory)
       .getPath))

(defn biomarkers-detail-path
  [analysis-result-directory component-name]
  (->> (io/file (str (digest/sha-1 component-name) ".json"))
       (io/file architectural-biomarkers-folder)
       (io/file analysis-result-directory)
       .getPath))

(defn- files->grouped-by-component
  [{:keys [analysis-path-fn] :as _context} file-name-fn transformations input-file]
  (when (seq transformations)
    (let [input-path (analysis-path-fn input-file)
          files-by-group (architectural-aggregator/files-by-component input-path file-name-fn transformations)
          mapped-files (architectural-aggregator/remove-unmapped-files files-by-group)]
      mapped-files)))

(defn- hotspots-with-revisions-by-component
  "Returns a map of component -> [hotspots]. The hotspots include
   the name and change frequency as we, later, need to get the head
    of the power law curve _relative_ to that component."
  [{:keys [analysis-path-fn architectural-transformations] :as _context}]
  (let [input-path (analysis-path-fn paths/hotspots-csv)
        spots (->> input-path
                   sc/slurp-csv
                   (sc/cast-with {:revisions sc/->int})
                   (map (fn [h] (dissoc h :id))))]
    (-> architectural-transformations
        architectural-aggregator/make-file->component-mapper
        (comp :module)
        (group-by spots)
        architectural-aggregator/remove-unmapped-files)))

(defn- prioritized-hotspots-by-component
  [{:keys [architectural-transformations] :as context}]
  (files->grouped-by-component context :name architectural-transformations paths/classified-spots-csv))

(defn notes-by-component
  [transformations note-risks]
  (let [notes-with-virtual-roots (map nu/note->note-with-virtual-root note-risks)]
    (-> transformations
        architectural-aggregator/make-file->component-mapper
        (comp :path)
        (group-by notes-with-virtual-roots)
        architectural-aggregator/remove-unmapped-files)))

(defn- architectural-component-names-from
  [{:keys [architectural-transformations] :as _context}]
  (->> architectural-transformations
       (map :transformation)
       distinct))

(defn- calculate-architectural-biomarkers?
  [{:keys [calculate-system-level-biomarkers] :as _project}
   {:keys [architectural-transformations] :as _context}]
  (and (some? calculate-system-level-biomarkers)
       calculate-system-level-biomarkers
       (seq architectural-transformations)))

(defn candidate-hotspots-from
  "We want to present a stable and predictable set of biomarkers on the
   head of the power law distribution of change frequencies."
  [hotspots repo-paths notes-risks]
  (->> hotspots
       (code-markers-analysis/filter-resolved-hotspots-paths repo-paths notes-risks)
       candidate-selection/head-of-power-law
       (map :module)))

(defn- component-notes-from
  "When creating the lookup table for notes based on component name, we introduce
   an extra level of indirection. Remove that here."
  [notes component-name]
  (map :note (get notes component-name [])))

(defn- score-component
  "This function is based a bunch of maps from component name -> data.
   Extract the data (hotspots, notes, etc) for each component and run
   the code health scoring for that component.
   Note that the last step is identical to how it is performed on a system-level, only
   difference is the filtered input data which is limited to the files matching the
   given architectural component."
  [component-name project context cached-scores notes ranked-hotspots prioritized-spots]
  (let [component-notes (component-notes-from notes component-name)
        component-spots (get ranked-hotspots component-name [])
        component-prioritized-spots (get prioritized-spots component-name [])
        component-candidate-spots (candidate-hotspots-from component-spots (:repo-paths project) component-notes)]
    (log/info "Calculating architectural biomarkers for component = " component-name)
    (code-markers-analysis/detailed-biomarkers-for project
                                                   context
                                                   cached-scores
                                                   component-notes
                                                   component-candidate-spots
                                                   component-prioritized-spots)))

(defn- component-with-score-path
  [analysis-path c]
  {:component c
   :score-path (score-path analysis-path c)
   :details-path (biomarkers-detail-path analysis-path c)})

(defn- write-scores-for-all
  "Generates two files for each architectural component:
    - one CSV with the raw scores of each hotspot, and
    - one JSON file with all the code biomarker indications."
  [components
   project
   {:keys [architectural-transformations analysis-path] :as context}
   cached-scores
   notes-risks-file]
  (let [notes-risks (nu/read-notes-risks notes-risks-file)
        notes (notes-by-component architectural-transformations notes-risks)
        ranked-hotspots (hotspots-with-revisions-by-component context)
        prioritized-spots (prioritized-hotspots-by-component context )]
    (doseq [component components]
      (let [{:keys [file-by-score details]}  (score-component component project context cached-scores notes ranked-hotspots prioritized-spots)
            score-destination (score-path analysis-path component)
            details-destination (biomarkers-detail-path analysis-path component)]
        (log/debug "System Health: calculated scores for " component)
        (io/make-parents score-destination) ; ensure that the path exists
        (sc/spit-csv score-destination file-by-score)
        (with-open [out-details (io/writer details-destination)]
          (json/write details out-details))))
    (map (partial component-with-score-path analysis-path) components)))

(defn- top-hotspots-from
  "When aggregating architectural components, we might come across the situation in small codebases where there's
  relatively little change and, hence, many hotspots get scored for each small component. This might bias the
  results, so only pick the top hotspots here."
  [spots]
  (take 5 spots))

(defn- aggregated-trend-scores-for
  [{:keys [component score-path details-path]}]
  (let [spots (code-markers-analysis/parse-biomarkers-from score-path)
        biomarkers-detail-lookups (code-markers-analysis/parse-biomarker-details-indexed-by-file details-path)
        current (code-markers-analysis/biomarker-average-for :score (top-hotspots-from spots) biomarkers-detail-lookups)
        last-month (code-markers-analysis/biomarker-average-for :last-month (top-hotspots-from spots) biomarkers-detail-lookups)
        last-year (code-markers-analysis/biomarker-average-for :last-year (top-hotspots-from spots) biomarkers-detail-lookups)]
    (log/debug "Architectural Score for " component ": current = " current ", last month = " last-month ", last year = " last-year)
    {:name component
     :current current
     :month last-month
     :year last-year}))

(defn- add-team-autonomy-measure
  [team-arch-level-ownership-csv-file fragmentation-arch-level-csv-file scored-components]
  (let [teams (parse/knowledge team-arch-level-ownership-csv-file)
        team-fragmentation (social-information/parse-fragmentation fragmentation-arch-level-csv-file)]
    (map (fn [{:keys [name] :as score}]
           (let [autonomy (social-information/team-autonomy-from teams team-fragmentation name)]
             (assoc score :teamautonomy autonomy)))
         scored-components)))

(defn- add-system-mastery-measure
  [knowledge-loss-file scored-components]
  (let [knowledge-loss (parse/knowledge-loss knowledge-loss-file)]
    (map (fn [{:keys [name] :as score}]
           (let [mastery (social-information/system-mastery-of knowledge-loss name)]
             (assoc score :mastery mastery)))
         scored-components)))

(defn- defect-trend-logs
  "Defects are calculated for two time windows: this month and last month.
   Typically, this month's defects are displayed and last month's value used
   to indicate trends. Both times are sliding windows based on the last known commit."
  [{:keys [age-time-now]} architectural-commits-log]
  (let [time-now (dates/string->date age-time-now)
        last-month (tc/minus time-now (tc/months 1))
        architectural-commits (sc/slurp-csv architectural-commits-log)]
    {:this-month (commit-date-filter/by-date-range {:filter-start-date last-month} architectural-commits)
     :last-month (commit-date-filter/by-date-range {:filter-start-date (tc/minus last-month (tc/months 1))
                                                    :filter-end-date last-month} architectural-commits)}))

(defn- add-defect-trends
  [project context architectural-commits-log scored-components]
  (let [{:keys [this-month last-month]} (defect-trend-logs context architectural-commits-log)
        defects-lookup-this-month (defect-density/defect-statistics-lookup-for-architectural-components project this-month)
        defects-lookup-last-month (defect-density/defect-statistics-lookup-for-architectural-components project last-month)]
    (map (fn [{:keys [name] :as score}]
           (-> score
               (assoc :defects (defects-lookup-this-month name))
               (assoc :defectsmonth (defects-lookup-last-month name))))
         scored-components)))

(defn- lookupable-revs-from-name
  [hotspots-file]
  (->> hotspots-file
       sc/slurp-csv
       (sc/cast-with {:revisions sc/->int})
       (map (juxt :module :revisions))
       (into {})))

(defn- add-revisions
  [hotspots-file scored-components]
  (let [hotspots (lookupable-revs-from-name hotspots-file)]
    (map (fn [{:keys [name] :as score}]
           (let [revs (get hotspots name 0)]
             (assoc score :revisions revs)))
         scored-components)))

(defn- cache-scores-for-known-hotspots
  "At this stage we have already calculated the scores for the
   top hotspots in the system. Since the scoring is expensive from a
   run-time perspective, we cache the known scores in a lookup table
   where we can fetch them rather that re-calculate the same files again."
  [{:keys [analysis-path-fn] :as _context}]
  (let [spots (code-markers-analysis/parse-all-biomarker-scores-from (analysis-path-fn paths/code-bio-marker-scores-csv))
        score-lookup (group-by :name spots)
        details-lookups (code-markers-analysis/parse-biomarker-details-indexed-by-file (analysis-path-fn paths/code-bio-marker-details-json))]
    {:score-cache score-lookup
     :details-cache details-lookups}))

(defn aggregate-code-health-scores
  [project
   context
   hotspots-file
   architectural-commits-log
   team-arch-level-ownership-csv-file
   fragmentation-arch-level-csv-file
   knowledge-loss-arch-level-csv-file
   notes-risks-file
   risk-by-commits-file
   aggregated-scores-dest-file]
  (if (calculate-architectural-biomarkers? project context)
    (let [components (architectural-component-names-from context)]
      (log/info "Calculating System Hotspots Health for " (count components) " logical components.")
      (let [cached-scores (cache-scores-for-known-hotspots context)
            scored-components-result-paths (write-scores-for-all components project context cached-scores notes-risks-file)
            scored-components (->> scored-components-result-paths
                                   (map aggregated-trend-scores-for)
                                   (add-team-autonomy-measure team-arch-level-ownership-csv-file fragmentation-arch-level-csv-file)
                                   (add-system-mastery-measure knowledge-loss-arch-level-csv-file)
                                   (add-defect-trends project context architectural-commits-log)
                                   (add-revisions hotspots-file)
                                   (architectural-risks/add-rolling-delivery-risk-for context risk-by-commits-file architectural-commits-log)
                                   (sort-by :revisions >))] ; present the hottest component first
        (sc/spit-csv aggregated-scores-dest-file scored-components)))
    (do
      (log/info "System Hotspots Health is not available and will be skipped"))))