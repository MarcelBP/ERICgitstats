(ns codescene.delta.warnings
  (:require [evolutionary-metrics.mergers.shared-mergers :as shared]
            [evolutionary-metrics.trends.complexity-trend :as trends]
            [codescene.analysis.paths :as paths]
            [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [codescene.analysis.hotspot-complexity-trends :as hotspot-complexity-trends]
            [clojure.java.io :as io]
            [evolutionary-metrics.trends.complexity-trend-step-detection :as complexity-trend-step-detection]
            [codescene.mining.evolution-accessor :as evolution]
            [evolutionary-metrics.mining.vcs :as vcs]
            [codescene.thresholds.analysis-thresholds :as analysis-thresholds]
            [taoensso.timbre :as log]
            [semantic-csv.core :as sc]
            [hotspots-x-ray.recommendations.code-health-markers :as markers]
            [codescene.biomarkers.code-ownership :as code-ownership]
            [hotspots-x-ray.recommendations.code-biomarkers-trend-detectors :as biomarker-trend-detectors]
            [codescene.note-watch.utils :as nu]))

(defn- spots-in-repository
  "Filters the Hotspots based on the the
   given repository. We want to do this because the
   delta analysis only apply to commits in a single repo
   and different repos may have different risk profiles and quality."
  [repo file-names]
  (let [prelude (file-patterns/final-segment repo)]
    (filter (fn [{:keys [name]}] (clojure.string/starts-with? name prelude)) file-names)))

(defn- relavant-modification?
  [v]
  (> (evolution/added-churn v) 1))

(defn- relevant-hotspots
  "We only want to give warnings on the most relevant hotspots, not the entire codebase.
   That's why we pick the top spots in the repository we investigate."
  [analysis-path-fn repo]
  (let [spots (->> (analysis-path-fn paths/augmented-hotspots-csv)
                   shared/read-csv-sans-header-from
                   (map (fn [r]
                          {:name (first r)
                           :hotspot-revisions (Integer/parseInt (second r))})))
        relevant-spots (spots-in-repository repo spots)
        n-spots-to-consider (min (Math/round (* (count relevant-spots) 0.02)) 100)]
    (take n-spots-to-consider relevant-spots)))

;;
;; Warnings for modified Hotspots
;;

(defn- modifies-hotspot
  [_context hotspots parsed-commits]
  (let [comparable-spots (->> hotspots (map :name) (into #{}))
        modifications-in-spots (filter (comp comparable-spots evolution/file-name) parsed-commits)]
    (->> modifications-in-spots
         (filter relavant-modification?)
         (map evolution/file-name)
         distinct)))

;;
;; Warnings for change in biomarker state
;;

(defn- version-from-git-with-custom-mechanism
  [mechanism {:keys [hotspot-name rev date] :as _modifcation} {:keys [git-cmd repo] :as _context}]
  (second (mechanism git-cmd repo
                     {:date date
                      :rev rev
                      :name (->> hotspot-name file-patterns/de-scope second)})))

(defn- delta-version-from-git
  [modificaiton context]
  (version-from-git-with-custom-mechanism vcs/lazy-historic-file-version modificaiton context))

(defn- direct-delta-version-from-git
  [modificaiton context]
  (version-from-git-with-custom-mechanism vcs/historic-file-version-of modificaiton context))

(defn- pick-latest
  "Given a seq of maps where each map represents a revision of file, we
   pick the one with the latest modification date."
  [modifications]
  (->> modifications
       (sort-by :basic-date-time) ; `:date` is not enough if two versions were commited on the same day
       reverse
       first))

(defn most-recent-modifications-by-file
  "Handle the case where the same file is modified twice in a pull request by
   picking the latest revision"
  [parsed-commits]
  (->> parsed-commits
       (map (fn [m]
              {:hotspot-name (evolution/file-name m)
               :rev (evolution/revision m)
               :date (evolution/revision-date m)
               :basic-date-time (:basic-date-time m)}))
       (group-by :hotspot-name)
       vals
       (map pick-latest)))

(def ^:const no-biomarkers-used [])

(defn- known-biomarkers
  [analysis-path-fn]
  (let [p (analysis-path-fn paths/code-bio-marker-scores-csv)]
    (if (.exists (io/as-file p))
      (->> p
           sc/slurp-csv
           (sc/cast-with {:score sc/->int
                          :calculated sc/->boolean})
           (filter :calculated))
      no-biomarkers-used)))

(defn- modified-files-with-known-markers
  [commits ms]
  (let [modified (->> commits
                      (map (fn [{:keys [hotspot-name] :as c}]
                             [hotspot-name c]))
                      (into {}))]
    (->> ms
         (map (fn [m] (merge m (get modified (:name m) {}))))
         (remove (comp nil? :hotspot-name)))))

(defn- hardcoded-congestion
  []
  {:authors       1
   :fractal-value 0.0})

(defn- score-delta-version
  [project context {:keys [name] :as modification}]
  (let [delta-version (direct-delta-version-from-git modification context)
        input (:content delta-version)]
    (markers/score name
                   project
                   {:congestion-score-fn     hardcoded-congestion
                    :historic-trend-score-fn biomarker-trend-detectors/no-historic-trend-score
                    :ownership-fn            code-ownership/no-code-ownership}
                   input)))

(defn- present-degreded-file
  [{:keys [name score old-score]}]
  (str (file-patterns/final-segment name)
       " degrades from a Code Health of " old-score " -> " score))

(defn- extend-file-with-old-score
  [lookupable-scores {:keys [name] :as s}]
  (assoc s :old-score (get lookupable-scores name)))

(defn- file-degrades-in-score?
  [{:keys [old-score score]}]
  (< score old-score))

(defn- degrading-files
  [lookupable-scores scored-deltas]
  (->> scored-deltas
       (map (partial extend-file-with-old-score lookupable-scores))
       (filter file-degrades-in-score?)
       (map present-degreded-file)))

(defn- degrades-biomarker-state
  [{:keys [use-biomarkers lookupable-known-scores scored-deltas] :as _context}
   _hotspots
   _parsed-commits]
  (if (true? use-biomarkers)
    (try
      (do
        (log/info "Proceeding to check biomarkers for modified hotspots: " (pr-str (map :name scored-deltas)))
        (degrading-files lookupable-known-scores scored-deltas))
      (catch Exception e
        (do
          (log/error "Failed to detect degrading biomarker." e)
          no-biomarkers-used)))
    no-biomarkers-used))

;;
;; Warnings for commits that violates a goal as defined by the notes (refactor, supervise, no problem).
;;

(def ^:const no-goals-defined [])

(defn- present-violated-goals
  [categorized-notes {:keys [name score old-score]}]
  (str "Hotspots marked " (get categorized-notes name "with note") ", "
       (file-patterns/final-segment name)
       ", degrades from a Code Health of " old-score " -> " score))

(defn- violates-goal
  "Checks for any degradation in note state compared to the last full analysis.
   Note that if a goal was already violated, then we don't report it here; we're
   only interested in new degradations."
  [{:keys [analysis-path-fn use-biomarkers lookupable-known-scores scored-deltas] :as _context}
   _hotspots
   _parsed-commits]
  (let [notes-risks (nu/read-notes-risks (analysis-path-fn paths/risks-for-notes-json))
        categorized-notes (nu/notes-with-category notes-risks)
        files-with-notes (set (keys categorized-notes))]
    (if (true? use-biomarkers)
      (let [scored-deltas-of-interest (filter (comp files-with-notes :name) scored-deltas)]
        (log/info "Delta check of goal violation for the notes: " (clojure.string/join ", " files-with-notes))
        (->> scored-deltas-of-interest
             (map (partial extend-file-with-old-score lookupable-known-scores))
             (filter file-degrades-in-score?)
             (map (partial present-violated-goals categorized-notes))))
      no-goals-defined)))

;;
;; Warnings for absence of Temporal Coupling
;;

(defn- temporal-couples-by-commit
  [analysis-path-fn degree-to-consider]
  (->> (analysis-path-fn paths/temporal-csv)
       shared/read-csv-sans-header-from
       (remove (fn [[_e1 _e2 degree]]
                 (> degree-to-consider (Integer/parseInt degree))))
       (map (fn [[p1 p2]] [p1 p2]))))

(def ^:private default-coupling-threshold-percent 80)

(defn acc-coupling-by-entity
  [acc p1 p2]
  ((fnil #(assoc acc p1 (conj % p2)) #{}) (get acc p1)))

(defn- coupling-by-entity
  [couples]
  (reduce (fn [acc [p1 p2]]
            (let [acc1 (acc-coupling-by-entity acc p1 p2)]
              (acc-coupling-by-entity acc1 p2 p1))) ; reflective
          {}
          couples))

(defn- expected-couples
  [candidates couples]
  (let [lookupable (coupling-by-entity couples)]
    (->> candidates
         (mapcat lookupable)
         (into #{}))))

(defn- misses-historic-change-pattern
  "Identifies change sets where an expected temporal coupling is absent.
   The reasoning is that of a cluster of files have changed together, they are
   intimately related. This warning fires when one such change pattern is broken.
   Please note that this may be good - we've refactored something - but it may
   also be a sign of omission and a potential bug.
   Note that this warning is a self-correcting algorithm; If you keep ignoring the
   warning it will go away automatically as the temporal coupling decreases below
   the thresholds."
  [{:keys [analysis-path-fn coupling-threshold-percent]
    :as _context}
   _hotspots
   parsed-commits]
  (let [couples (temporal-couples-by-commit
                 analysis-path-fn
                 (or coupling-threshold-percent default-coupling-threshold-percent))
        modified-files (map evolution/file-name parsed-commits)
        couple-names (->> couples flatten (into #{}))
        candidates (filter couple-names modified-files)
        expected (expected-couples candidates couples)]
    (clojure.set/difference expected (set modified-files))))

;;
;; Complexity trend warnings
;;

(defn- spots-with-trend-files
  [{:keys [version analysis-path]} hotspots]
  (->> hotspots
       (map (fn [{:keys [hotspot-name] :as s}]
              (merge s
                     {:analysis-result-directory analysis-path
                      :qualified-name hotspot-name})))
       (map (fn [s] (assoc s :filename (hotspot-complexity-trends/complexity-trend-path version s))))
       (filter (fn [{:keys [filename]}] (.exists (io/as-file filename)))))) ; the trends may not exist for all Hotspots depending on configuration

(defn- delta-complexity-for
  [{:keys [hotspot-name] :as modifcation} context]
  (let [delta-version (delta-version-from-git modifcation context)
        language-dependent-rules (trends/language-rules-for hotspot-name)
        complexity-trend (first (trends/trend [delta-version] language-dependent-rules))]
    complexity-trend))

(defn- combine-complexity
  [known-trend-file suggested-complexity]
  (let [ds-fn (complexity-trend-step-detection/complexity-dataset-from-file known-trend-file)
        added-complexity-row (complexity-trend-step-detection/row->complexity-dataset suggested-complexity)]
    (concat ds-fn [added-complexity-row])))

(defn- as-step-detection-format
  [context {:keys [hotspot-name hotspot-revisions filename] :as modification}]
  (let [suggested-complexity (delta-complexity-for modification context)]
    {:hotspot-name                hotspot-name
     :hotspot-revisions           hotspot-revisions
     :complexity-trend-dataset-fn (partial combine-complexity filename suggested-complexity)}))

(defn- extend-trend-with-new-modification
  [candidates context]
  (map (partial as-step-detection-format context) candidates))

(defn- detect-exceeded-thresholds-for
  [{:keys [project time-now analysis-path-fn] :as context} candidates]
  (let [hotspots-log (analysis-path-fn paths/hotspots-csv)
        hotspot-stats (analysis-thresholds/describe-hotspots hotspots-log)
        warning-thresholds (analysis-thresholds/complexity-trend-warning-thresholds hotspot-stats)
        thresholds (hotspot-complexity-trends/as-complexity-thresholds-configuration warning-thresholds project)
        calculateable-candidates (extend-trend-with-new-modification candidates context)
        complexity-status (complexity-trend-step-detection/detect-increasing-steps-in calculateable-candidates
                                                                                      thresholds
                                                                                      time-now)
        resolved-candidates (map merge candidates complexity-status)]
    (log/debug "Delta detects complexity trend warnings with thresholds: " (pr-str warning-thresholds))
    (filter (fn [{:keys [complexity-trend-classification]}]
              (some (partial < 0) complexity-trend-classification))
            resolved-candidates)))

(defn- triggers-complexity-warning
  [context hotspots parsed-commits]
  (let [modifications (most-recent-modifications-by-file parsed-commits)]
    (let [spot-name->revision (->> hotspots (map (juxt :name :hotspot-revisions)) (into {}))
          spots-with-commit-info (map (fn [m]
                                        (assoc m :hotspot-revisions (get spot-name->revision (:hotspot-name m) 0)))
                                      modifications)
          candidates (spots-with-trend-files context spots-with-commit-info)
          warnings (detect-exceeded-thresholds-for context candidates)]
      (vec (map :hotspot-name warnings)))))

;;
;; The biomarkers are expensive to calculate (run-time cost). At the same time they are
;; needed for two different warnings. We solve this by calculating them up front -- if configured to use
;; biomarkers -- and provide them in the context to each warning detector.
;;

;; TODO: fetch notes too!!
;; TODO: fetch the arch biomarker files too so we get more candidates!

(defn- all-notes-on-biomarker-score-format
  [analysis-path-fn]
  (let [notes-risks (nu/read-notes-risks (analysis-path-fn paths/risks-for-notes-json))]
    (nu/notes-biomarker-score notes-risks)))

(defn- extend-biomarkers-with
  [note-scores ms]
  (let [known-spots (->> ms (map :name) set)]
    (->> note-scores
         (remove (comp known-spots :name))
         (concat ms))))

(defn- know-biomarkers-for-modified-files
  "Create a cache where all previously (i.e. last full analysis) scored files are included.
   Note that we cannot just use the biomarker files as some notes (e.g. 'no problem') won't be
   included there. Hence, we fetch all known biomarkers and all known notes together with their scores."
  [project
   {:keys [analysis-path-fn use-biomarkers] :as context}
   parsed-commits]
  (if (true? use-biomarkers)
    (try
      (let [ms (known-biomarkers analysis-path-fn)
            note-scores (all-notes-on-biomarker-score-format analysis-path-fn)
            all-candidates (extend-biomarkers-with note-scores ms)
            delta-versions (most-recent-modifications-by-file parsed-commits)
            ms-of-interest (modified-files-with-known-markers delta-versions all-candidates)
            scored-deltas (map (partial score-delta-version project context) ms-of-interest)]
        {:scored-deltas scored-deltas
         :lookupable-known-scores (->> all-candidates
                                       (map (juxt :name :score))
                                       (into {}))})
      (catch Exception e
        (do
          (log/error "Failed to calculate delta biomarker." e)
          {})))
    {}))

;;
;; Public API
;;

(defn- combine-warnings
  [project detectors context hotspots parsed-commits]
  (let [biomarkers (know-biomarkers-for-modified-files project context parsed-commits)
        extended-context (merge context biomarkers)]
    (->> detectors
         (map (fn [{:keys [detector-fn category]}] {:category category
                                                    :result (detector-fn extended-context hotspots parsed-commits)}))
         (filter (comp seq :result))
         (map (fn [{:keys [category result]}] {:category category
                                               :details (into [] result)})))))

(def ^:private modified-hotspot-warning-category "Modifies Hotspot")
(def ^:private complexity-trend-warning-category "Complexity Trend Warning")
(def ^:private absence-of-change-warning-category "Absence of Expected Change Pattern")
(def ^:private degrades-in-code-health-warning-category "Degrades in Code Health")
(def ^:private violates-goal-warning-category "Violates Goals")

(def ^:private warning-detectors
  [{:detector-fn modifies-hotspot            :category modified-hotspot-warning-category}
   {:detector-fn triggers-complexity-warning :category complexity-trend-warning-category}
   {:detector-fn misses-historic-change-pattern :category absence-of-change-warning-category}
   {:detector-fn degrades-biomarker-state :category degrades-in-code-health-warning-category}
   {:detector-fn violates-goal :category violates-goal-warning-category}])

(defn- triggers-quality-gate-named
  [gate-name detected-warnings]
   (->> detected-warnings
        (filter (comp (partial = gate-name) :category))
        seq
        some?))

(defn- triggered-quality-gates
  [detected-warnings]
  {:degrades-in-code-health (triggers-quality-gate-named degrades-in-code-health-warning-category detected-warnings)
   :violates-goal (triggers-quality-gate-named violates-goal-warning-category detected-warnings)})

(defn detect
  [project {:keys [analysis-path-fn] :as context} repo parsed-commits]
  (log/info "Running detectors using the context: " (pr-str context))
  (let [spots (relevant-hotspots analysis-path-fn repo)
        ws (combine-warnings project warning-detectors context spots parsed-commits)]
    {:warnings ws
     :quality-gates (triggered-quality-gates ws)}))

