(ns codescene.note-watch.supervised-scores
  (:require [clojure.java.io :as io]
            [cheshire.core :as json]
            [cheshire.generate :refer [add-encoder encode-str]]
            [taoensso.timbre :as log]
            [clj-time.coerce :as timec]
            [clj-time.format :as f]
            [codescene.analysis.paths :as paths]
            [codescene.biomarkers.code-markers-analysis :as cma]
            [hotspots-x-ray.recommendations.code-health-markers :as markers]
            [evolutionary-metrics.mining.vcs :as vcs]
            [evolutionary-metrics.trends.dates :as dates]
            [evolutionary-metrics.trends.complexity-trend :as ctrend]
            [codescene.note-watch.renames :as renames]
            [hotspots-x-ray.recommendations.code-biomarkers-trend-detectors :as biomarker-trend-detectors]
            [hotspots-x-ray.content-resolver :as resolver]
            [evolutionary-metrics.mining.ws-complexity :as wsc]
            [codescene.biomarkers.code-ownership :as code-ownership]))

(defn file-details-for-note
  [note]
  (let [file (or (:new-entity note) (:last-entity note)) ; tricky: if renamed, new-entity contains the name otherwise last-entity.
        resolved (resolver/as-child-path (:repo-path note) file)]
    {:repo-path (:repo-path note)
     :relative-name file
     :resolved resolved
     :name file
     :note note}))

(defn version-from-git-with-custom-mechanism
  [{:keys [repo-path]} {:keys [git-client]} {:keys [rev date name]}]
  (second (vcs/lazy-historic-file-version git-client repo-path
                                          {:date date
                                           :rev rev
                                           :name name})))

(defn- revision-start-date-for
  [{:keys [git-client] :as _context}
   {:keys [analysis-start-date] :as _project}
   {:keys [note] :as file-details}]
  (let [commit-date (vcs/date-of-commit git-client (:repo-path file-details) (:original-rev note))]
    (if (some? (:success commit-date))
      (:date commit-date)
      (do
        (log/warn "Cannot fetch author date of rev = " (:original-rev note) " for " (:name file-details) ". Defaulting to analysis start date.")
        analysis-start-date))))

(defn- trace-internal-issues
  [{:keys [original-rev]} file-name original]
  (when-not (:exists? original)
    (log/warn "Notes: Couldn't resolve original note revision: " original-rev " for file: " file-name)))

(defn- original-revision-from
  [{:keys [original-rev original-entity] :as _note} revision-date]
  {:exists? true
   :match {:date (dates/string->date revision-date)
           :rev original-rev
           :name original-entity}})

(def ^:private no-trends-available {:original 0, :new 0})

(defn- score-biomarker-trends-for
  [context
   {:keys [developer-alias-map] :as project}
   file-details
   original-rev]
  (if (markers/supports-file-type? (:name file-details))
    (let [original-score (cma/historic-biomarker-state-for cma/empty-project-to-supress-comments-analysis-in-trends file-details developer-alias-map context original-rev)
          new-score (cma/score-current-file-in context project file-details biomarker-trend-detectors/no-historic-trend-score code-ownership/no-code-ownership)]
      {:original (:score original-score)
       :new (:score new-score)})
    no-trends-available))

(defn- lines-of-code-in
  [content lang-rule]
  (let [{:keys [n]} (wsc/total-indent-complexity ctrend/options
                                                 content
                                                 lang-rule)]
    n))

(defn- loc-in-file-on-disk
  [f lang-rule]
  (with-open [rdr (io/reader f)]
    (lines-of-code-in (line-seq rdr) lang-rule)))

(defn- score-code-growth-trends-for
  [context {:keys [resolved] :as file-details} original-rev]
  (let [lang-rule (ctrend/language-rules-for (:name file-details))
        old-score (lines-of-code-in
                    (:content (version-from-git-with-custom-mechanism file-details context original-rev))
                    lang-rule)
        new-score (loc-in-file-on-disk resolved lang-rule)]
    {:original old-score :new new-score}))

(defn note-trend-measures
  [context
   project
   {:as file-details, :keys [note]}]
  (let [start-date-for-note (revision-start-date-for context project file-details)
        original-rev (original-revision-from note start-date-for-note)
        biomarkers (score-biomarker-trends-for context project file-details original-rev)
        trends (score-code-growth-trends-for context file-details (:match original-rev))]
    (trace-internal-issues note (:relative-name file-details) original-rev)
    {:scores {:biomarkers biomarkers
              :trends trends}
     :note note}))

(defn supervised-scores-for-notes!
  [{:as project, :keys [repo-paths]} context output-dir notes]
  (let [new-notes (renames/new-names-for-notes! repo-paths notes output-dir)
        files-details (map file-details-for-note (:found new-notes))
        notes-scores (map (partial note-trend-measures context project) files-details)
        scores-file-name (paths/as-child-path output-dir "supervised-scores-for-notes.json")]
    (log/debug "Persisting notes scores: " scores-file-name)
    (with-open [out-file (io/writer scores-file-name)]
      (json/generate-stream notes-scores out-file))
    (assoc new-notes :scores notes-scores)))

(defn biomarkers-exists?
  [biomarkers]
  (not= (and (:original biomarkers) (:new biomarkers)) 0))

;; maybe should be defined in some cfg ??
(def supervised-trend-threshold 1.25)
(def no-problem-trend-threshold 2)
(def refactor-trend-threshold 0.05)
(def default-steady-refactor-threshold-ms (* 30 24 60 60 1000)) ;; one month

(defn- no-change-in-refactor-biomarkers?
  [biomarkers]
  (= (:new biomarkers) (:original biomarkers)))

(defn- are-refactor-biomarkers-problem?
  [biomarkers threshold time-difference]
  (or (< (:new biomarkers) (:original biomarkers))
      (and (no-change-in-refactor-biomarkers? biomarkers)
           (< threshold time-difference))))

(defn- are-supervised-biomarkers-ok?
  [biomarkers]
  (>= (:new biomarkers) (:original biomarkers)))

(defn- are-no-problems-biomarkers-ok?
  [biomarkers]
  (>= (:new biomarkers) (:original biomarkers)))

(defn- no-change-in-refactor-trends?
  [trends]
  (let [t (* refactor-trend-threshold (:original trends))]
    (<= (Math/abs (- (:new trends) (:original trends))) t)))

(defn- are-refactor-trends-problem?
  [trends threshold time-difference]
  (let [t (* refactor-trend-threshold (:original trends))]
    (or (> (:new trends) (+ (:original trends) t))
        (and (no-change-in-refactor-trends? trends)
             (< threshold time-difference)))))

(defn- are-supervised-trends-ok?
  [trends]
  (<= (:new trends)
    (* supervised-trend-threshold (:original trends))))

(defn- are-no-problems-trends-ok?
  [trends]
  (<= (:new trends)
      (* no-problem-trend-threshold (:original trends))))

(def ^:private date-formatter (f/formatters :year-month-day))

(defn analyze-note-risk
  [time-now steady-refactor-threshold-ms note-score]
  (let [{:keys [biomarkers trends]} (-> note-score :scores)
        steady-refactor-threshold (or steady-refactor-threshold-ms default-steady-refactor-threshold-ms)
        time-difference (- (timec/to-long time-now)
                           (->> note-score :note :created (f/parse date-formatter) timec/to-long))
        biomarkers-warning (if (biomarkers-exists? biomarkers)
                             (case (-> note-score :note :category)
                               "refactor"
                               (if (are-refactor-biomarkers-problem? biomarkers
                                                                     steady-refactor-threshold
                                                                     time-difference)
                                 "warning"
                                 (if (no-change-in-refactor-biomarkers? biomarkers)
                                   "no-change"
                                   "ok"))

                               "supervise"
                               (if (are-supervised-biomarkers-ok? biomarkers) "ok" "warning")

                               "no-problem"
                               (if (are-no-problems-biomarkers-ok? biomarkers)  "ok" "warning"))
                             "not-calculated")
        trends-warning (case (-> note-score :note :category)
                         "refactor"
                         (if (are-refactor-trends-problem? trends
                                                           steady-refactor-threshold
                                                           time-difference)
                           "warning"
                           (if (no-change-in-refactor-trends? trends)
                             "no-change"
                             "ok"))

                         "supervise"
                         (if (are-supervised-trends-ok? trends) "ok" "warning")

                         "no-problem"
                         (if (are-no-problems-trends-ok? trends) "ok" "warning"))]
    {:warnings {:biomarkers-warning biomarkers-warning ;; visualisation should be red
                :trends-warning trends-warning}        ;; visualisation should be yellow
     :note-score note-score}))

(defn analyze-risks-for-notes!
  [project context output-dir notes]
  (let [notes-with-scores (supervised-scores-for-notes! project context output-dir notes)
        note-scores (:scores notes-with-scores)
        risks (mapv (partial analyze-note-risk (:time-now context)
                             (:steady-refactor-threshold-ms project))
                    note-scores)
        risks-file-name (paths/as-child-path output-dir "risks-for-notes.json")]
    (log/debug "Persisting notes risks: " risks-file-name)
    (with-open [out-file (io/writer risks-file-name)]
      (json/generate-stream risks out-file))
    (assoc notes-with-scores :risks risks)))
