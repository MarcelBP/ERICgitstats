(ns codescene.reports.report
  (:require [codescene.reports.pdf-reports :as pdf-reports]
            [clj-pdf.core :as pc :only [pdf]]
            [codescene.analysis.paths :as paths]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [hotspots-x-ray.recommendations.code-health-markers :as code-health]
            [clojure.spec.alpha :as s]
            [codescene.reports.pdf-reports-spec :as reports-spec]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clj-time.local :as tl]
            [clj-time.format :as tf]
            [cheshire.core :as cheshire]
            [codescene.biomarkers.code-markers-analysis :as code-markers-analysis]
            [taoensso.timbre :as log]
            [codescene.analysis.analysis-warnings :as analysis-warnings]
            [codescene.branches.branch-statistics :as branch-statistics]
            [clojure.data.json :as json]
            [codescene.biomarkers.architectural-level-hotspots :as architectural-hotspots]
            [codescene.note-watch.supervised-scores :as supervised-scores]
            [clojure.string :as str]))

;; TODO: Put these simple generic functions somewhere else... (or find equivalents)

(defn read-json [json-file]
  (with-open [reader (io/reader json-file)]
    (json/read reader :key-fn keyword)))

(defn read-optional-json [json-file default]
  (if (.exists (io/as-file json-file))
    (read-json json-file)
    default))

(defn read-json-cheshire [json-file]
  (with-open [reader (io/reader json-file)]
    (cheshire/parse-stream reader true)))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            repeat)
       (rest csv-data)))

(defn csv->maps [csv-file]
  (with-open [reader (io/reader csv-file)]
    (doall (csv-data->maps (csv/read-csv reader)))))

(defn optional-csv->maps [csv-file]
  (if (.exists (io/as-file csv-file))
    (csv->maps csv-file)
    []))

(defn csv->map [csv-file]
  (with-open [reader (io/reader csv-file)]
    (->> (csv/read-csv reader)
         (map (fn [[k v]] [(keyword k) v]))
         (into {}))))

(defn optional-csv->map [csv-file]
  (if (.exists (io/as-file csv-file))
    (csv->map csv-file)
    {}))

(defn- parse-int [s]
  (Integer/parseInt s))

(defn- parse-nilable-int [s]
  (when (and s (not= s "-"))
    (Integer/parseInt s)))

(defn- parse-nilable-percentage [s]
  (when (and s (not= s "-"))
    (/ (Integer/parseInt s) 100.0)))

(defn- parse-nilable-string [s]
  (when (and s (not= s "-"))
    s))

(defn- parse-autonomy [s]
  (if (some? s)
    (get {"High"   :high-team-autonomy
          "Medium" :medium-team-autonomy
          "Low"    :low-team-autonomy
          "-"      :not-calculated} s)
    :not-calculated))

(defn- parse-positive-int [s]
  (when-let [v (parse-nilable-int s)]
    (when (> v 0)
      v)))

(defn- parse-nilable-float [s]
  (when (and s (not= s "-"))
    (Double/parseDouble s)))

;;TODO: Copy-pasted fn fron enterprise -> refactor!
(defn- highest-delivery-risk-for
  [system-hotspots-health]
  (->> system-hotspots-health
       (map :deliveryrisk)
       (map parse-nilable-int)
       (filter some?)
       (sort >)
       first))

(defn- notes-for [notes file]
  (->> notes
       (map supervised-scores/file-details-for-note)
       (filter #(string/includes? (:resolved %) file))
       (map :note)))

(defn- file-details-for-note->goal [file-details-for-note]
  {:file         (:relative-name file-details-for-note)
   :goal-details (get-in file-details-for-note [:note :note-text])})

(defn- missed-goals-of-category [category file-details-for-note notes-with-warnings]
  (let [missed-ids (->> notes-with-warnings (map :id) (into #{}))
        note-id #(get-in % [:note :id])]
    (->> file-details-for-note
         (filter #(missed-ids (note-id %)))
         (filter :resolved)
         (filter #(= category (get-in % [:note :category])))
         (map file-details-for-note->goal))))

(defn- note-text-for [notes]
  (str/join "\n" (map :note-text notes)))

(defn system-health [dashboard risks-for-notes system-hotspots-health classified-hotspot-stats analysis-time]
  (let [notes-warnings (analysis-warnings/note-specific-warnings-in risks-for-notes)
        hotspots (first (filter #(= (:title %1) "Hotspots") classified-hotspot-stats))]
    {:abandoned-code           (parse-nilable-percentage (:knowledgelosspercentage dashboard))
     :active-authors           (parse-nilable-int (:activeauthors dashboard))
     :commits-last-month       (parse-nilable-int (:commitslastmonth dashboard))
     :former-developers        (parse-nilable-int (:knowledgelossauthors dashboard))
     :code-health              (parse-positive-int (:biomarkercurrent dashboard))
     :last-month-code-health   (parse-positive-int (:biomarkermonthback dashboard))
     :last-year-code-health    (parse-positive-int (:biomarkeryearback dashboard))
     :lines-of-code            (parse-nilable-string (:codesize dashboard))
     :analysis-time            analysis-time
     :missed-goals             (::analysis-warnings/supervision-warnings notes-warnings)
     :total-goals              (::analysis-warnings/total-goals notes-warnings)
     :delivery-risk            (highest-delivery-risk-for system-hotspots-health)
     :defect-ratio-in-hotspots (parse-nilable-percentage (:defectratioinhotspots dashboard))
     :red-hotspot-ratio        (when hotspots (/ (first (:measures hotspots)) 100.0))
     :red-hotspot-effort       (when hotspots (/ (first (:efforts hotspots)) 100.0))
     :key-personnel            (parse-nilable-int (:nkeypersonnel dashboard))
     :key-personnel-ratio      (parse-nilable-percentage (:nkeypersonnelpercentage dashboard))}))

(defn early-warnings [risk-by-commit-early-warnings risks-for-notes]
  ;; TODO:  Go from commit,risk,author,date,repository to title,description,early-warning-details (list)
  [])

(defn- goals [notes risks-for-notes]
  (let [note-warnings (analysis-warnings/note-specific-warnings-in risks-for-notes)
        notes-with-warnings (analysis-warnings/notes-with-warnings-in risks-for-notes)
        file-details-for-notes (map supervised-scores/file-details-for-note notes)]
    {:missed-goals        (::analysis-warnings/supervision-warnings note-warnings)
     :total-goals         (::analysis-warnings/total-goals note-warnings)
     :missed-refactorings (missed-goals-of-category "refactor" file-details-for-notes notes-with-warnings)
     :missed-supervisions (missed-goals-of-category "supervise" file-details-for-notes notes-with-warnings)
     :missed-no-problems  (missed-goals-of-category "no-problem" file-details-for-notes notes-with-warnings)}))

(defn- branch-statistics-item->delivery-risk [early-warning]
  {:branch-name    (:name early-warning)
   :lead-time      (parse-nilable-int (:leadmerge early-warning))
   :risk           (parse-nilable-int (:risk early-warning))
   :nbr-of-commits (parse-nilable-int (:commits early-warning))})

(defn has-warning? [branch]
  (or (not= (:authorwarning branch) branch-statistics/no-warning)
      (not= (:ttlwarning branch) branch-statistics/no-warning)))

(defn delivery-risks [branch-statistics]
  (->> branch-statistics
       (filter #(= "true" (:calculated %)))
       (map branch-statistics-item->delivery-risk)))

(defn- revision-trend-item->development-output [item]
  {:date             (:date item)
   :commits-per-week (parse-int (:revisions item))
   :authors-per-week (parse-int (:authors item))})

(defn- authors-per-month-item->active-authors [item]
  {:date              (:date item)
   :authors-per-month (parse-int (:authors item))})

(defn system-trends [revision-trend authors-per-month-trend]
  {:development-output-over-time (map revision-trend-item->development-output revision-trend)
   :active-authors-over-time     (map authors-per-month-item->active-authors authors-per-month-trend)})

(defn- code-health-for-a-human->hotspot-detail [detail]
  {:severity    (:indication detail)
   :description (:title detail)})

(defn- biomarkers-details-entry-for [code-biomarkers-details file]
  (->> (:markers code-biomarkers-details)
       (filter #(= (:name %) file))
       first))

(defn- hotspot-details-for [code-biomarkers-details file]
  (let [entry (biomarkers-details-entry-for code-biomarkers-details file)]
    (if (some? entry)
      (map code-health-for-a-human->hotspot-detail (code-health/interpret-for-a-human entry))
      [])))

(defn- code-bio-marker-score->system-hotspot [notes code-biomarkers-details code-bio-marker-score]
  (let [file (:name code-bio-marker-score)]
    {:file                   file
     :code-health            (parse-positive-int (:score code-bio-marker-score))
     :last-month-code-health (parse-positive-int (:last-month code-bio-marker-score))
     :last-year-code-health  (parse-positive-int (:last-year code-bio-marker-score))
     :hotspot-details        (hotspot-details-for code-biomarkers-details file)
     :comment                (note-text-for (notes-for notes file))}))

(defn- system-hotspot-health->subsystem-health [code-bio-marker-score]
  {:sub-system             (:name code-bio-marker-score)
   :team-autonomy          (parse-autonomy (:teamautonomy code-bio-marker-score))
   :system-mastery         (parse-nilable-percentage (:mastery code-bio-marker-score))
   :code-health            (parse-positive-int (:current code-bio-marker-score))
   :last-month-code-health (parse-positive-int (:month code-bio-marker-score))
   :last-year-code-health  (parse-positive-int (:year code-bio-marker-score))
   :defects                (parse-nilable-int (:defects code-bio-marker-score))
   :last-defects           (parse-nilable-int (:defectsmonth code-bio-marker-score))
   :delivery-risk          (parse-nilable-int (:deliveryrisk code-bio-marker-score))})


(defn- system-hotspots [notes code-bio-marker-scores code-bio-marker-details-json]
  (->> code-bio-marker-scores
       (filter #(= "true" (:calculated %)))
       (map (partial code-bio-marker-score->system-hotspot notes code-bio-marker-details-json))))

(defn- subsystem-health [system-hotspots-health]
  (map system-hotspot-health->subsystem-health system-hotspots-health))

(defn- hotspots-for-subsystem [analysis-results-path-fn notes component]
  (let [analysis-result-directory (analysis-results-path-fn "")
        code-bio-marker-scores-csv (architectural-hotspots/score-path analysis-result-directory component)
        code-bio-marker-scores (csv->maps code-bio-marker-scores-csv)
        code-bio-marker-details-json (architectural-hotspots/biomarkers-detail-path analysis-result-directory component)
        code-bio-marker-details (read-json code-bio-marker-details-json)]
    (system-hotspots notes code-bio-marker-scores code-bio-marker-details)))


(defn- hotspots-by-subsystem [analysis-results-path-fn notes system-hotspots-health]
  (let [components (map :name system-hotspots-health)]
    (->> components
         (map (fn [c] [c (hotspots-for-subsystem analysis-results-path-fn notes c)]))
         (into {}))))

(defn- architect-data [report-spec]
  (let [{:keys [path-fn notes]} report-spec
        code-bio-marker-scores (csv->maps (path-fn paths/code-bio-marker-scores-csv))
        code-biomarkers-details (read-json (path-fn paths/code-bio-marker-details-json))
        ;;risk-by-commit-early-warnings (csv->maps (path-fn paths/risk-by-commit-early-warnings-csv))
        risks-for-notes (read-optional-json (path-fn paths/risks-for-notes-json) [])
        system-hotspots-health (optional-csv->maps (path-fn paths/system-hotspots-health-csv))]
    {:subsystem-health      (subsystem-health system-hotspots-health)
     ;;:early-warnings        (early-warnings risk-by-commit-early-warnings risks-for-notes)
     :goals                 (goals notes risks-for-notes)
     :system-hotspots       (system-hotspots notes code-bio-marker-scores code-biomarkers-details)
     :hotspots-by-subsystem (hotspots-by-subsystem path-fn notes system-hotspots-health)}))

(defn- qa-data [report-spec]
  (let [{:keys [path-fn]} report-spec
        branch-statistics (csv->maps (path-fn paths/branch-statistics-csv))
        system-hotspots-health (optional-csv->maps (path-fn paths/system-hotspots-health-csv))
        risk-by-commit-early-warnings (csv->maps (path-fn paths/risk-by-commit-early-warnings-csv))
        risks-for-notes (read-optional-json (path-fn paths/risks-for-notes-json) [])]
    {:subsystem-health (subsystem-health system-hotspots-health)
     :delivery-risks   (delivery-risks branch-statistics)
     :early-warnings   (early-warnings risk-by-commit-early-warnings risks-for-notes)}))

(defn- manager-data [report-spec]
  (let [{:keys [path-fn notes analysis-time]} report-spec
        authors-per-month-trend (csv->maps (path-fn paths/authors-per-month-trend))
        dashboard (optional-csv->map (path-fn paths/dashboard-csv))
        revision-trend (csv->maps (path-fn paths/revision-trend-csv))
        risks-for-notes (read-optional-json (path-fn paths/risks-for-notes-json) [])
        system-hotspots-health (optional-csv->maps (path-fn paths/system-hotspots-health-csv))
        classified-hotspot-stats (read-optional-json (path-fn paths/classified-spot-stats-csv) [])]
    {:system-health (system-health dashboard risks-for-notes system-hotspots-health classified-hotspot-stats analysis-time)
     :goals         (goals notes risks-for-notes)
     :system-trends (system-trends revision-trend authors-per-month-trend)}))

(defn- management-data [report-spec]
  (let [{:keys [path-fn analysis-time]} report-spec
        dashboard (optional-csv->map (path-fn paths/dashboard-csv))
        risks-for-notes (read-optional-json (path-fn paths/risks-for-notes-json) [])
        system-hotspots-health (optional-csv->maps (path-fn paths/system-hotspots-health-csv))
        classified-hotspot-stats (read-optional-json (path-fn paths/classified-spot-stats-csv) [])]
    {:system-health (system-health dashboard risks-for-notes system-hotspots-health classified-hotspot-stats analysis-time)}))

(defn- management-datas [report-specs-by-project]
  (into {} (map
             (fn [[project-name report-spec]] [project-name (management-data report-spec)])
             report-specs-by-project)))

(defn for-architect [report-spec options out]
  (pdf-reports/project-report (architect-data report-spec) options out))

(defn for-qa [report-spec options out]
  (pdf-reports/project-report (qa-data report-spec) options out))

(defn for-manager [report-spec options out]
  (pdf-reports/project-report (manager-data report-spec) options out))

(defn for-management [report-specs-by-project options out]
  (pdf-reports/management-report (management-datas report-specs-by-project) options out))
  
