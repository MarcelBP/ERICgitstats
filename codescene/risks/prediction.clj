(ns codescene.risks.prediction
  (:require [codescene.risks.predictor-mining :as mining]
            [codescene.analysis.paths :as paths]
            [clj-time.core :as tc-core]
            [clojure.java.io :as java-io]
            [clojure.data.csv :as csv]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [evolutionary-metrics.trends.rolling-average :as rolling]
            [incanter.core :as incanter]
            [incanter.stats :as incanter-stats]
            [evolutionary-metrics.trends.rolling-average]
            [evolutionary-metrics.parsers.time-parser :as tp]
            [clj-time.core :as tc]
            [taoensso.timbre :as log]))

(defn risk-regression-for
  "We had an unexpected failure. While tracking that down (some value here was nil), I added
   the safe-guard below"
  [diffusion experience churn]
  (+ (* 0.41  (Math/log (or diffusion 0.01)))
     (* -0.11 (Math/log (or experience 0.01)))
     (* 0.18  (Math/log (inc (or churn 1))))))

(defn raw-risk-of-single-commit
  "Provides a risk classification based on a linear regression model as defined in
   the paper Predicting Risk of Software Changes (Mockus & Weiss, 2000)."
  [{:keys [experience-lookup diffusion-lookup churn-lookup commit-info-lookup] :as _predictors} commit-hash]
  (let [{:keys [author date repository]} (get commit-info-lookup commit-hash)
        experience (get experience-lookup author)
        diffusion (get diffusion-lookup commit-hash)
        churn (get churn-lookup commit-hash)]
    (when-not (some? experience)
      (log/warn "Risk prediction not possible for commit hash " commit-hash " since the author " author " isn't known. A problem with the alias mapping?"))
    [commit-hash (risk-regression-for diffusion experience churn) author date repository]))

(def ^:private n-risk-classes 10)

(defn- safe-risk-values
  [vs]
  (if (empty? vs)
    [0.0]
    vs))

(defn risk-categorizer-from
  "Adjusts a raw risk value (the result of the regression) to a
   categorized value in the range 1-10.
   The categorizes are calculated from the range [lowest risk, highest risk]."
  [raw-risks]
  (let [risk-values (->> raw-risks (map second) safe-risk-values)
        highest (apply max risk-values)
        lowest (apply min risk-values)
        risk-span (- highest lowest)
        ticks (/ risk-span n-risk-classes)
        cut-off->class (map (fn [idx]
                              (let [cut-off (+ lowest (* ticks idx))]
                                [cut-off idx]))
                            (range n-risk-classes))]
    (fn [[commit-hash risk & commit-details]]
      (if-let [match (some (fn [[cut-off klass]]
                             (when (> cut-off risk)
                               klass))
                           cut-off->class)]
        (into [commit-hash match] commit-details)
        (into [commit-hash n-risk-classes] commit-details))))) ; corner case: our maximum risk

(defn as-ordinal-risks
  "Classifies each commit as a risk between 1 - 10.
   The classification is always _relative_ to the maximum, historic risk in
   the codebase."
  [predictors commit-hashes]
  (let [raw-risks (map (partial raw-risk-of-single-commit predictors) commit-hashes)
        categorizer-fn (risk-categorizer-from raw-risks)]
    (->> raw-risks
         (map categorizer-fn)
         (incanter/dataset [:commit :risk :author :date :repository])
         (incanter/$order [:date :risk :commit] :desc)
         incanter/to-vect)))

(def ^:private default-n-days-rolling-risk-window 14)

(defn- incanter-group-by
  [group-criterion ds]
  (if (zero? (incanter/nrow ds))
    []
    (incanter/$group-by group-criterion ds)))

(defn- risks-per-day
  [risks]
  (let [ds (incanter/dataset [:commit :risk :author :date :repository] risks)]
       (for [[day-group daily-commits] (incanter-group-by :date ds)
          :let [date (:date day-group)
                risks (rolling/fix-single-return-value-bug (incanter/$ :risk daily-commits))
                avg-risk (incanter-stats/mean risks)
                total-risk (reduce + risks)]]
      [date avg-risk total-risk])))

(defn- rolling-average-risk
  [risks n-rolling-days]
  (as-> risks $
        (risks-per-day $)
        (incanter/dataset [:date :avgrisk :totalrisk] $)
        (incanter/$order :date :asc $)
        (rolling/as-rolling-average-of $ n-rolling-days :avgrisk :rollingrisk)
        (incanter/$order :date :asc $)
        (incanter/to-vect $)))

(defn diffusion-after-inital-day
  "Some codebases come to life with an intial, gigantic commit that
   imports an existing system. Such commits are outliers that will
   impact the risk classification. Here we use a crude way of getting
   rid of them: we drop all commits during the first day of the codebase.
   This applies to all codebases (because it's simpler that way) and won't
   influence the overall risk much in other cases."
  [diffusions]
  (if-let [oldest-commit (last diffusions)]
    (let [first-day (nth oldest-commit 1)]
      (remove (fn [[_commit date]]
                (= date first-day))
              diffusions))
    diffusions))

(defn- predictors-from-analysis-files
  [author-stats-file diffusion-file]
  (let [authors (shared/read-csv-sans-header-from author-stats-file)
        diffusion (diffusion-after-inital-day (shared/read-csv-sans-header-from diffusion-file))]
    {:experience-lookup    (mining/author-experience-lookup authors)
     :diffusion-lookup     (mining/commit-diffusion-lookup diffusion)
     :churn-lookup         (mining/commit-loc-added-lookup diffusion)
     :commit-info-lookup   (mining/commit-info-lookup diffusion)}))

(defn- write-to-disk
  [headings data destination]
  (with-open [out-file (java-io/writer destination)]
    (csv/write-csv out-file headings)
    (csv/write-csv out-file data)))

(defn- write-commit-risks-to-disk
  [risks risk-destination]
  (write-to-disk [["commit" "risk" "author" "date" "repository"]] risks risk-destination))

(defn risk-by-commit
  ([{:keys [analysis-path-fn]} project]
   (let [risk-window (get-in project [:risk :rolling-days] default-n-days-rolling-risk-window)
         author-stats-file (analysis-path-fn paths/author-churn-csv)
         diffusion-file (analysis-path-fn paths/diffusion-by-revision-csv)
         {:keys [commit-info-lookup] :as predictors} (predictors-from-analysis-files author-stats-file diffusion-file)
         commit-hashes (keys commit-info-lookup) ; we can use any predictor we want for this - it's the same info
         risks (as-ordinal-risks predictors commit-hashes)
         rolling-risk (rolling-average-risk risks risk-window)]
     {:risks risks
      :rolling-risk rolling-risk}))
  ([{:keys [analysis-path-fn] :as context} project {:keys [risk-dest-file-name rolling-risk-dest-file-name]}]
   (let [risk-destination (analysis-path-fn risk-dest-file-name)
         rolling-risk-destination (analysis-path-fn rolling-risk-dest-file-name)
         {:keys [risks rolling-risk]} (risk-by-commit context project)]
     (write-commit-risks-to-disk risks risk-destination)
     (write-to-disk [["date" "averagerisk" "totalrisk" "rollingrisk"]] rolling-risk rolling-risk-destination))))


;; Early warnings
;;

(def ^:private time-parser-fn (tp/time-parser-from tp/internal-time-format-string))

(defn as-time-object
  [t]
  (time-parser-fn t))

(defn- within-time-frame
  [cut-off-date commits]
  (filter (fn [c]
            (not (tc-core/before? (-> c (nth 3) as-time-object) cut-off-date)))
          commits))

(defn- above-warning-threshold
  [warning-threshold commits]
  (filter (fn [[_commit risk]]
            (>= (Integer/parseInt risk) warning-threshold))
          commits))

(defn predictions-matching
  "Used to calculate early warnings based on specific criterions"
  ([{:keys [analysis-path-fn]} {:keys [risk-dest-file-name warning-class warning-days]} time-now]
   (let [all-commits (->> risk-dest-file-name analysis-path-fn shared/read-csv-sans-header-from)
         cut-off-date (tc-core/minus time-now (tc/days warning-days))]
     (->> all-commits
          (within-time-frame cut-off-date)
          (above-warning-threshold warning-class))))
  ([{:keys [analysis-path-fn] :as context} thresholds time-now warning-dest-file-name]
   (let [warning-destination (analysis-path-fn warning-dest-file-name)
         warnings (predictions-matching context thresholds time-now)]
     (write-commit-risks-to-disk warnings warning-destination)
     warnings)))

