(ns codescene.biomarkers.code-markers-analysis
  (:require [hotspots-x-ray.recommendations.code-health-markers :as markers]
            [hotspots-x-ray.recommendations.developer-congestion :as congestion]
            [clojure.data.json :as json]
            [semantic-csv.core :as sc]
            [taoensso.timbre :as log]
            [clj-time.core :as tc]
            [hotspots-x-ray.content-resolver :as resolver]
            [hotspots-x-ray.recommendations.code-biomarkers-trend-detectors :as biomarker-trend-detectors]
            [evolutionary-metrics.mining.vcs :as vcs]
            [evolutionary-metrics.trends.dates :as dates]
            [codescene.stats.math :as math]
            [codescene.note-watch.utils :as nu]
            [clojure.set :as cset]
            [codescene.biomarkers.candidate-selection :as candidate-selection]
            [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [codescene.biomarkers.code-ownership :as code-ownership]
            [codescene.analysis.paths :as paths]))

;; Code Biomarkers
;; ===============
;;
;; We need a way to show progress and indicate what kind of problems CodeScene sees in the hotspots.
;; I'd like to avoid stuff like "quality" or "maintainability" since they are so easy to game and suggest and absolute truth.
;; Instead, we go with the concept of Code Biomarkers.
;;
;; From Wikipedia:
;;    "In medicine, a biomarker is a measurable indicator of the severity or presence of some disease state.
;;     More generally a biomarker is anything that can be used as an indicator of a particular disease state or
;;     some other physiological state of an organism."
;;
;; In CodeScene, out biomarkers aim to indicate specific diseases in the code.

;; Generate biomarker trends
;; ==========================
;;
;; Each biomarker includes its previous state, a month and a year ago.
;; The end-user value is that they get to see the trend in code quality for the prioritized hotspots.
;; For example, "Your current hotspots are marked as C. Last month you were at B and a year ago it was also a B", which
;; indicate code that's degrading in quality.

(defn with-resolved-file-name
  [repo-paths qualified-name]
  (let [[path name] (resolver/file-in-project-repository-paths-from repo-paths qualified-name)
        p (resolver/as-child-path path name)]
    {:name qualified-name
     :resolved p
     :repo-path path
     :relative-name name}))

(defn filter-resolved-hotspots-paths
  [repo-paths notes-risks all-hotspots]
  (let [n (+ candidate-selection/max-hotspots-to-take (count notes-risks))
        head (take n all-hotspots)
        tail (drop n all-hotspots)]
    (concat (filter (fn [hotspot]
                      (->> hotspot
                           :module
                           (with-resolved-file-name repo-paths) ;; virtual-path -> (repo, relative-path-in-repo) transformation
                           (nu/non-resolved-hotspot? notes-risks)))
                    head)
            tail)))

(defn candidate-hotspots-from
  "We want to present a stable and predictable set of biomarkers on the
   head of the power law distribution of change frequencies."
  [hotspots repo-paths notes-risks]
  (->> hotspots
       (sc/cast-with {:revisions sc/->int})
       (filter-resolved-hotspots-paths repo-paths notes-risks)
       candidate-selection/head-of-power-law
       (map :module)))

(defn- younger?
  [date rev]
  (not (tc/after? (:date rev) date)))

(defn pick-revision-at
  [date file-revisions]
  (let [older (take-while (partial younger? date) file-revisions)
        match (last older)]
    {:exists? (some? match)
     :match match}))

(defn pick-revisions-for-trend-from
  "The file-versions are sorted with the oldest version first."
  [file-revisions time-now]
  (let [last-month (tc/minus time-now (tc/months 1))
        last-year (tc/minus time-now (tc/years 1))]
    {:last-month (pick-revision-at last-month file-revisions)
     :last-year (pick-revision-at last-year file-revisions)}))

(defn- score-congestion-for
  [{:keys [repo-path relative-name]} {:keys [git-client] :as _context} developer-alias-map score-date]
  (congestion/monthly-contributing-devs git-client repo-path score-date relative-name developer-alias-map))

(def ^:const no-historic-file-version 0)

(def ^:const no-historic-file-details
  {:score no-historic-file-version})

(defn historic-biomarker-state-for
  [project
   {:keys [repo-path relative-name] :as file-target}
   developer-alias-map
   {:keys [git-client] :as context} {:keys [exists? match] :as _revision}]
  (log/debug "Historic biomarker for " relative-name " at " (:date match) " revision " (:rev match))
  (if exists?
    (let [[fetched? input] (vcs/historic-file-version-of git-client repo-path match)
          congestion-scorer (partial score-congestion-for file-target context developer-alias-map (:date match))]
      (if fetched?
        (->> input :content (markers/score relative-name
                                           project
                                           {:congestion-score-fn     congestion-scorer
                                            :historic-trend-score-fn biomarker-trend-detectors/no-historic-trend-score
                                            :ownership-fn code-ownership/no-code-ownership}))
        no-historic-file-details))
    no-historic-file-details))

(defn- trend-for-unsupported-language
  [file-details]
  {:last-month no-historic-file-version
   :last-year no-historic-file-version
   :file-details file-details})

(defn- trend-measures-dates-for
  [{:keys [git-client age-time-now] :as _context} {:keys [analysis-start-date] :as _project} file-details]
  (let [metadata (vcs/historic-file-versions-metadata git-client
                                                      (:repo-path file-details)
                                                      analysis-start-date
                                                      (:relative-name file-details))
        file-revisions (map (fn [r] (assoc r :date (dates/string->date (:date r)))) metadata)]
    (pick-revisions-for-trend-from file-revisions (dates/string->date age-time-now))))

(defn- cached-score-for
  [{:keys [score-cache details-cache]} {:keys [name]}]
  (when (and details-cache
             score-cache)
    (let [{:keys [score]} (get score-cache name)
          details (get details-cache name)]
      (when (and details
                 score)
        {:score score
         :name name
         :details details}))))

(defn- cached-historic-scores-for
  [{:keys [score-cache]} {:keys [name] :as file-details}]
  (when (some? score-cache)
    (let [{:keys [last-month last-year]} (get score-cache name)]
      (when (and last-month
                 last-year)
        {:last-month last-month
         :last-year last-year
         :file-details file-details}))))

(def ^:const empty-project-to-supress-comments-analysis-in-trends {})

(defn- add-trend-measures
  [context
   {:keys [developer-alias-map] :as project}
   cached-scores
   file-details]
  (if (markers/supports-file-type? (:name file-details))
    (if-let [cached-historic-score (cached-historic-scores-for cached-scores file-details)]
      cached-historic-score
      (let [{:keys [last-month last-year]} (trend-measures-dates-for context project file-details)
            biomarkers-last-month (historic-biomarker-state-for empty-project-to-supress-comments-analysis-in-trends file-details developer-alias-map context last-month)
            biomarkers-last-year (historic-biomarker-state-for empty-project-to-supress-comments-analysis-in-trends file-details developer-alias-map context last-year)]
        (log/debug "Scoring trend measures for " (:name file-details))
        {:last-month (:score biomarkers-last-month)
         :last-month-details biomarkers-last-month
         :file-details file-details
         :last-year (:score biomarkers-last-year)}))
    (do
      (log/debug "Ignoring trend measures for " (:name file-details) " as the type isn't supported")
      (trend-for-unsupported-language file-details))))

(defn- score-for-unsupported-language
  [{:keys [name] :as file}]
  {:name name
   :score no-historic-file-version
   :details []
   :calculated false
   :file-details file})

(defn score-current-file-in
  [{:keys [age-time-now] :as context}
   {:keys [developer-alias-map] :as project}
   {:keys [name resolved] :as file-details}
   historic-trend-score-fn
   code-ownership-fn]
  (if (and (some? name)
           (markers/supports-file-type? name))
    (try
      (let [input (slurp resolved)
            congestion-scorer (partial score-congestion-for file-details context developer-alias-map (dates/string->date age-time-now))
            score (markers/score name
                                 project
                                 {:congestion-score-fn     congestion-scorer
                                  :historic-trend-score-fn historic-trend-score-fn
                                  :ownership-fn code-ownership-fn}
                                 input)]
        (log/debug "Start scoring " name)
        (if (= markers/no-score (:score score)) ; a module without any code, possibly a RSA-RTE model
          (score-for-unsupported-language file-details)
          (merge score {:calculated true :file-details file-details})))
      (catch Throwable e
        (log/error (str "Failed to calculate code biomarkers for " name) e)
        (score-for-unsupported-language file-details)))
    (score-for-unsupported-language file-details)))

(defn score-file-in
  [context
   project
   cached-scores
   code-ownership-fn
   {:keys [last-month-details file-details] :as historic-trend-scores}]
  (if-let [cached (cached-score-for cached-scores file-details)]
    (merge cached historic-trend-scores)
    (let [score (score-current-file-in context
                                       project
                                       file-details
                                       (partial biomarker-trend-detectors/historic-trends-from last-month-details)
                                       code-ownership-fn)]
      (-> score
          (merge historic-trend-scores)
          (dissoc :last-month-details)))))

(def ^:private no-markers-available [])

(defn- score-when-configured
  [{:keys [analysis-path-fn] :as context}
   {:keys [repo-paths calculate-code-biomarkers] :as project}
   cached-scores
   hotspots]
  (if calculate-code-biomarkers
    (try
      (log/info "Calculating code biomarkers")
      (let [hotspots-paths (map (partial with-resolved-file-name repo-paths) hotspots)
            ownership-fn (code-ownership/code-ownership-from (analysis-path-fn paths/social-main-dev) (analysis-path-fn paths/knowledge-loss-csv))]
        (->> hotspots-paths
             (map (partial add-trend-measures context project cached-scores))
             (map (partial score-file-in context project cached-scores ownership-fn))))
      (catch Throwable e
        (log/error "Failed to calculate code biomarkers" e) ; just swallow it since we want the rest of the analysis to run
        no-markers-available))
    no-markers-available))

(defn- as-csv-format
  [candidate? note-score? score]
  (let [n (:name score)
        candidate (if (candidate? n) 1 0)
        presentable (if (or (candidate? n)
                            (note-score? n)) 1 0)]
    (-> score
        (dissoc :details)
        (dissoc :file-details)
        (assoc :candidatescore candidate)
        (assoc :presentable presentable))))

(defn- tuple-for-cmp
  [notes-severity score]
  [(nu/note-severity-score notes-severity (:name score))
   (:score score)])

(defn- sort-scores
  [notes-risks all-scores]
  (let [notes-severity (nu/notes-severity notes-risks)]
    (sort-by (partial tuple-for-cmp notes-severity) all-scores)))

(def ^:const no-cached-scores {})

(defn detailed-biomarkers-for
  [project context cached-scores notes-risks candidate-spots prioritized-spots]
  (let [prioritized-that-are-not-candidates (remove (set candidate-spots) prioritized-spots)
        hotspots-with-rf-or-su-notes (-> notes-risks nu/notes-hotspot-candidates set)
        notes-candidates (cset/difference hotspots-with-rf-or-su-notes
                                          (cset/union (set candidate-spots)
                                                      (set prioritized-that-are-not-candidates)))
        candidate-spot-scores (score-when-configured context project cached-scores candidate-spots)
        priortized-spot-scores (score-when-configured context project cached-scores prioritized-that-are-not-candidates)
        notes-scores (score-when-configured context project cached-scores notes-candidates)
        result (sort-scores notes-risks
                            (concat notes-scores candidate-spot-scores priortized-spot-scores))
        file-by-score (map (partial as-csv-format (set candidate-spots) hotspots-with-rf-or-su-notes) result)
        details {:version 1 :markers result}]
    {:file-by-score file-by-score
     :details details}))

(defn parse-all-biomarker-scores-from
  [trend-file]
  (->> trend-file
       sc/slurp-csv
       (sc/cast-with {:score sc/->int
                      :last-month sc/->int
                      :last-year sc/->int
                      :calculated sc/->boolean
                      :candidatescore sc/->int})))

(defn parse-biomarkers-from
  [trend-file]
  (->> trend-file
       parse-all-biomarker-scores-from
       (filter :calculated)
       (filter (comp (partial = 1) :candidatescore))))

(defn parse-biomarker-details-indexed-by-file
  [details-json-file]
  (with-open [reader (clojure.java.io/reader details-json-file)]
    (let [r (json/read reader :key-fn keyword)]
      (doall
        (->> r
             :markers
             (map (juxt :name :details))
             (into {}))))))

; Biomarker scores are weighted depending on the size of the hotspot in Loc.
; We do that by multiplying the number of scores by how many large code blocks each
; hotspot contain. The size of a (logical) code block is defined below.
; So, any hotspot with less that X LoC will have its score considered once. A hotspot
; with X * 3 code blocks will have its score counted three times, and so on.
(def ^:private weight-block-in-loc 500)

(defn- weight-score-by-loc
  [file->biomarker-details field f]
  (if-let [d (file->biomarker-details (:name f))]
    (let [{:keys [active-code-size]} d
          loc-weight (if (some? active-code-size) active-code-size weight-block-in-loc)
          score-weight (max (int (/ loc-weight weight-block-in-loc)) 1)]
      (repeat score-weight (field f)))
    [(field f)])) ; shouldn't happen but default to an unweighted score

; avoid that better looking code further down skews the results.
(def ^:private max-hotspots-for-health 7)

(defn- no-score-available?
  [field spot]
  (let [score (field spot)]
    (and (int? score)
         (= 0 score))))

(defn biomarker-average-for
  [field ms file->biomarker-details]
  (->> ms
       (remove (partial no-score-available? field))
       (take max-hotspots-for-health)
       (map (partial weight-score-by-loc file->biomarker-details field))
       (apply concat)
       math/stats-for
       :mean
       Math/floor
       Math/round))
