(ns codescene.analysis.dashboard-generator
  (:require [codescene.analysis.paths :as paths]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [codescene.biomarkers.code-markers-analysis :as code-biomarkers]
            [codescene.analysis.defect-density :as defects]
            [codescene.queries.key-personnel-detection :as key-personnel]
            [codescene.stats.math :as math]
            [incanter.stats :as stats]
            [codescene.stats.conversions :as conversions]
            [semantic-csv.core :as sc]
            [evolutionary-metrics.analysis.math :as evo-math]
            [taoensso.timbre :as log]
            [evolutionary-metrics.trends.dates :as dates]
            [clj-time.core :as tc]
            [medley.core :as medley]))

;;; This module is responsible for generating the information we
;;; want to present on the results dashboard.

(defn- summary-by-keys
  [result-file]
  (map (fn [row] [(first row) (Integer/parseInt (last row))])
       (shared/read-csv-sans-header-from result-file)))

(def ^:private format-large-number (partial pprint/cl-format nil "~:d"))

(defn- code-metrics
  [result-file _]
  (let [by-language (summary-by-keys result-file)
        total (reduce + (map second by-language))
        primary-language (first by-language)]
    [["codesize" (format-large-number total)]
     ["primarylanguage" (if (some? primary-language) (first primary-language) "None")]
     ["primarylanguagesize" (format-large-number (if (some? primary-language) (second primary-language) 0))]]))

(defn- lookupable-commits
  [result-file]
  (into {} (summary-by-keys result-file)))

(defn- commit-metrics
  [result-file _]
  (let [commits-info (lookupable-commits result-file)
        commits (get commits-info "Commits")
        authors (get commits-info "Authors")
        active-authors (get commits-info "Active Authors")]
    [["numberofcontributions" commits]
     ["authors" authors]
     ["activeauthors" active-authors]]))

(defn- hotspot-metrics
  [result-file _]
  (let [spot-names (map first (shared/read-csv-sans-header-from result-file))
        spots-to-present (take 5 spot-names)]
    (map-indexed (fn [i n] [(str "hotspot" (inc i)) n]) spots-to-present)))

(defn- degree-coupling-of
  [couple]
  (when couple
    (nth couple 2)))

(defn- coupling-metrics
  [result-file _]
  (let [couples (shared/read-csv-sans-header-from result-file)
        unique-entities (distinct (mapcat (fn [couple] [(first couple) (second couple)]) couples))
        highest-coupling (first couples)
        lowest-coupling (last couples)]
    [["coupledentities" (count unique-entities)]
     ["coupledlower" (degree-coupling-of lowest-coupling)]
     ["coupledupper" (degree-coupling-of highest-coupling)]]))

(defn- coupling-metrics-prefixer
  "Allows us to re-use the coupling dashboard highlights for
   all different variations of temporal coupling."
  [prefix result-file context]
  (map (fn [[name stat]] [(str prefix name) stat])
       (coupling-metrics result-file context)))

(def ^:private architectural-coupling-metrics (partial coupling-metrics-prefixer "arch"))

(defn- percentage-of
  [v total]
  (-> (/ v (max total 1))
      (* 100)
      Math/ceil
      int))

(defn- knowledge-loss-percentage
  [knowledge-loss]
  (let [total (map (fn [[_entity main-dev added total-added]]
                     [main-dev (Integer/parseInt added) (Integer/parseInt total-added)])
                   (shared/read-csv-sans-header-from knowledge-loss))
        total-code (reduce + (map #(nth % 2) total))
        lost-knowledge (->> (filter #(= "Ex-Developers" (first %)) total) (map second) (reduce +))]
    (percentage-of lost-knowledge total-code)))

(defn- team-metrics
  [knowledge-loss {:keys [authors teams]}]
  (let [ex-devs (distinct (filter :exdev authors))]
    [["teams" (count teams)]
     ["markeddevelopers" (count (filter :color authors))]
     ["knowledgelossauthors" (count ex-devs)]
     ["knowledgelosspercentage" (knowledge-loss-percentage knowledge-loss)]]))

(defn- as-fragmented-entity-metrics
  [i entity]
  (let [name (first entity)
        n-authors (last entity)]
    [[(str "fragmented" (inc i)) name]
     [(str "fragmented" (inc i) "devs") n-authors]]))

(defn- fragmentation-metrics
  [result-file _]
  (let [fragmentations (shared/read-csv-sans-header-from result-file)
        fragmentations-to-consider (take 5 fragmentations)
        statistics (map-indexed as-fragmented-entity-metrics fragmentations-to-consider)]
    (apply concat statistics)))

(defn- author-contribution-time
  [churn]
  (map (fn [[_name _added _deleted _net _revs months]] (Integer/parseInt months)) churn))

(defn- ex-devs-median-experience
  [ex-dev-names churn]
  (->> churn
       (filter (comp ex-dev-names first))
       author-contribution-time
       stats/median))

(defn- author-churn-metrics
  [result-file {:keys [authors]}]
  (let [ex-dev-names (->> authors
                          (filter :exdev)
                          (map :name)
                          set)
        churn (shared/read-csv-sans-header-from result-file)
        contrib-times (author-contribution-time churn)
        median-contrib-time (if (empty? contrib-times) 0 (stats/median contrib-times))
        max-contrib-time (if (empty? contrib-times) 0 (apply max contrib-times))]
    [["mediancontribtime" median-contrib-time]
     ["maxcontribtime" max-contrib-time]
     ["exdevelopersmedianexperience" (ex-devs-median-experience ex-dev-names churn)]]))

(defn- architectural-metrics
  [_ {:keys [architectural-transformations]}]
  [["architecturaldefs" (count architectural-transformations)]])

(def ^:private component-coupling-metrics (partial coupling-metrics-prefixer "comp"))

(def ^:private component-commit-coupling-metrics (partial coupling-metrics-prefixer "compcommit"))

(defn- project-management-metrics
  [result-file {:keys [analysis-path-fn]}]
  (if (.exists (io/as-file result-file))
    (let [items (shared/read-csv-sans-header-from result-file)
          total-costs (->> items (map second) (map #(Integer/parseInt %)) (reduce +))
          cost-unit (->> paths/pm-data-description-csv analysis-path-fn shared/read-csv-sans-header-from (map second) first)]
      [["pmitems" (count items)]
       ;; TODO: Check cost unit and only apply readable-minutes for "minutes" type,
       ;; otherwise use custom format.
       ["pmtotalcosts" (if (= cost-unit "minutes")
                         (conversions/readable-minutes total-costs)
                         (format-large-number total-costs))]])
    [["pmitems" 0]
     ["pmtotalcosts" 0]]))

(defn- modus-operandi-metrics
  [_ {:keys [modus-commit-message-pattern]}]
  [["modusoperandimessagepattern" (or modus-commit-message-pattern "")]])

(defn- branch-metrics
  [_ {:keys [branches]}]
  (let [ttl-stats (some->> branches (map :ttl) math/stats-for)
        merge-stats (some->> branches (filter :merged) (map :leadmerge) math/stats-for)]
    [["nbranches" (if (seq branches) (count branches) 0)]
     ["branchttl" (if ttl-stats (conversions/readable-minutes (* 60 (:mean ttl-stats))) 0.0)]
     ["branchmergelead" (if merge-stats (conversions/readable-minutes (* 60 (:mean merge-stats))) 0.0)]]))

(defn- biomarkers
  [trend-file {:keys [analysis-path-fn] :as _context}]
  (let [spots (code-biomarkers/parse-biomarkers-from trend-file)]
    (if (seq spots)
      (let [details-file (analysis-path-fn paths/code-bio-marker-details-json)
            biomarkers-detail-lookups (code-biomarkers/parse-biomarker-details-indexed-by-file details-file)]
        [["nbiomarkers" (count spots)]
         ["biomarkercurrent" (code-biomarkers/biomarker-average-for :score spots biomarkers-detail-lookups)]
         ["biomarkermonthback" (code-biomarkers/biomarker-average-for :last-month spots biomarkers-detail-lookups)]
         ["biomarkeryearback" (code-biomarkers/biomarker-average-for :last-year spots biomarkers-detail-lookups)]])
      [["nbiomarkers" 0]
       ["biomarkercurrent" 0]
       ["biomarkermonthback" 0]
       ["biomarkeryearback" 0]])))

(defn- defect-density-metrics
  [defects-density-file _context]
  (let [n-total-defects (->> defects-density-file
                             sc/slurp-csv
                             (sc/cast-with {:defects sc/->int})
                             (map :defects)
                             (reduce +))]
    (if (> n-total-defects 0)
      [["ntotaldefects" n-total-defects]]
      [])))

(defn- defects-in-hotspots-statistics
  [hotspots-file {:keys [context project]}]
  (try ; we don't want to fail the analysis this late and we do have a UI fallback if this optional info is missing
    (let [spots (->> hotspots-file sc/slurp-csv (map :name))
          commits (sc/slurp-csv (:filtered-log context))
          {:keys [unique-defects defects-in-hotspots]} (defects/defect-statistics-for-hotspots project commits spots)]
      (if (and (> unique-defects 0)
                 (> defects-in-hotspots 0))
        (let [spot-ratio (int (evo-math/as-percentage (/ defects-in-hotspots unique-defects)))]
          [["defectratioinhotspots" spot-ratio]
           ["nuniquedefects" unique-defects]
           ["ndefectsinhotspots" defects-in-hotspots]])
        []))
    (catch Exception e
      (log/warn "Failed to generate hotspot vs defects statics" e)
      [])))

(defn revisions-over-last-month-from
  [commits {:keys [time-now] :as _context}]
  (let [cut-off-time (tc/minus time-now (tc/months 1))
        cs-over-last-month (->> commits
                                (medley/distinct-by :rev)
                                (remove (fn [{:keys [date]}]
                                          (tc/before? (dates/string->date date) cut-off-time)))
                                count)]
    [["commitslastmonth" cs-over-last-month]]))

(defn- revisions-over-last-month
  [cs-file {:keys [context]}]
  (revisions-over-last-month-from (sc/slurp-csv cs-file) context))

(defn- key-personnel-in-codebase
  [entity-ownership-file {:keys [context]}]
  (let [{:keys [analysis-path-fn]} context
        lost-authors-file (analysis-path-fn paths/lost-authors-csv)
        {:keys [n-key-personnel n-key-personnel-percentage]} (key-personnel/key-developer-statistics-from entity-ownership-file lost-authors-file)]
    [["nkeypersonnel" n-key-personnel]
     ["nkeypersonnelpercentage" n-key-personnel-percentage]]))

(def ^:private highlights
  [[code-metrics paths/file-summary-csv]
   [commit-metrics paths/summary-csv]
   [hotspot-metrics paths/hotspots-csv]
   [coupling-metrics paths/temporal-csv]
   [architectural-coupling-metrics paths/architectural-grouped-temporal-coupling-csv]
   [team-metrics paths/knowledge-loss-csv]
   [fragmentation-metrics paths/fragmentation-csv-file-name]
   [author-churn-metrics paths/author-churn-csv]
   [architectural-metrics "dummy-placeholder"]
   [component-coupling-metrics paths/architectural-level-coupling-csv]
   [component-commit-coupling-metrics paths/architectural-level-commit-coupling-csv]
   [project-management-metrics paths/pm-item-input-data-csv]
   [modus-operandi-metrics "dummy-placeholder"]
   [branch-metrics "dummy-placeholder"]
   [biomarkers paths/code-bio-marker-scores-csv]
   [defect-density-metrics paths/defect-density-by-file-csv]
   [defects-in-hotspots-statistics paths/classified-spots-csv]
   [revisions-over-last-month paths/technical-vcs-log]
   [key-personnel-in-codebase paths/entity-ownership-csv-file-name]])

(defn write-analysis-highlights
  "Writes a CSV with the analysis highlights that drive the dashboard information."
  [{:keys [analysis-path-fn architectural-transformations] :as context}
   {:keys [modus-commit-message-pattern] :as project}
   authors
   teams
   branches]
  (let [highlights-context {:analysis-path-fn              analysis-path-fn
                            :architectural-transformations architectural-transformations
                            :authors                       authors
                            :teams                         teams
                            :modus-commit-message-pattern  modus-commit-message-pattern
                            :branches                      branches
                            :context context
                            :project project}]
    (with-open [out-file (io/writer (analysis-path-fn paths/dashboard-csv))]
      (doseq [[highlight input] highlights]
        (csv/write-csv out-file (highlight (analysis-path-fn input) highlights-context))))))
