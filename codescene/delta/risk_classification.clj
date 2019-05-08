(ns codescene.delta.risk-classification
  (:require [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [codescene.analysis.paths :as paths]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [codescene.mining.cleaning :as cleaning]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [codescene.mining.cleaning :as cleaning]
            [codescene.risks.predictor-mining :as predictors]
            [codescene.risks.prediction :as risk]
            [evolutionary-metrics.analysis.revision-churn :as revision-churn]
            [taoensso.timbre :as log]))

(defn diffusion-for-repository
  "Filters the diffusion metrics based on the the
   given repository. We want to do this because the
   delta analysis only apply to commits in a single repo
   and different repo may have different risk profiles and quality."
  [repo diffusions]
  (filter (comp (partial = (file-patterns/final-segment repo)) last) diffusions))

(defn- distinct-entities-in
  [parsed]
  (->> parsed (map :entity) distinct count))

(defn- normalize-file-diffusion
  [diffusions parsed]
  (let [max-historic-value (->> diffusions (map #(Integer/parseInt (nth % 3))) cleaning/safe-max)
        commit-diffusion (distinct-entities-in parsed)]
    (cleaning/normalize max-historic-value commit-diffusion)))

(defn- loc-added-in
  [parsed]
  (->> parsed (map #(Integer/parseInt (:loc-added %))) (reduce +)))

(defn- normalize-churn
  [diffusions parsed]
  (let [max-historic-value (->> diffusions (map #(Integer/parseInt (nth % 5))) cleaning/safe-max)
        commit-loc (loc-added-in parsed)]
    (cleaning/normalize max-historic-value commit-loc)))

;; A bit tricky since there may be multiple authors in a
;; range of commits. We go with a mean value of their
;; contribution experience.
(defn- normalize-author-experience
  [author-experience-lookup parsed]
  (->> parsed
       (map :author)
       distinct
       (map author-experience-lookup)
       (remove nil?) ; tricky - a new and yet unknown author may have contributed
       cleaning/average))

(defn- risk-classifier-from
  [author-experience diffusions]
  (let [historic-predictors {:experience-lookup    author-experience
                             :diffusion-lookup     (predictors/commit-diffusion-lookup diffusions)
                             :churn-lookup         (predictors/commit-loc-added-lookup diffusions)
                             :commit-info-lookup   (predictors/commit-info-lookup diffusions)}
        commit-hashes (keys (:commit-info-lookup historic-predictors))
        raw-risks (map (partial risk/raw-risk-of-single-commit historic-predictors) commit-hashes)]
    (risk/risk-categorizer-from raw-risks)))

(defn- risk-of-commits
  [author-experience diffusions {:keys [commit-diffusion commit-experience commit-churn]}]
  (let [risk-catagorizer (risk-classifier-from author-experience diffusions)
        raw-commit-risk (risk/risk-regression-for commit-diffusion commit-experience commit-churn)
        [_commit ordinal _details] (risk-catagorizer ["hash" raw-commit-risk "details"])]
    ordinal))

(defn commits-of-interest?
  [parsed-commits]
  (and (seq parsed-commits)
       (revision-churn/revision-contains-logic-changes? parsed-commits)))

(defn- describe-experience-to-append
  [commit-experience]
  (let[low-experience? (< commit-experience 0.1)]
    (if low-experience? "The risk increases as the author has somewhat lower experience in this repository."
                        (if (> commit-experience 0.25)
                          "The risk is lower since it's a very experienced author with many contributions."
                          "The risk is somewhat lower due to an experienced author."))))

(defn- describe-low-risk-commit
  [commit-experience commit-diffusion commit-churn {:keys [files-touched added-loc]}]
  (let [low-diffusion?  (< commit-diffusion 0.1)
        experience-class (str ". "(describe-experience-to-append commit-experience))
        low-churn? (< commit-churn 0.1)]
    (cond
      (and low-diffusion? low-churn?)
      (str "The change is low risk as it touches less code than your typical change set")

      (and (not low-diffusion?) low-churn?)
      (str "The change is low risk, and touches " files-touched
           " files and modifies " added-loc " lines of code" experience-class)

      (and low-diffusion? (not low-churn?))
      (str "The change set modifies " added-loc " lines of code, and it seems to be a focused change with lower risk" experience-class)

      (and (not low-diffusion?) (not low-churn?))
      (str "The change is low risk, and modifies " added-loc " lines of code in " files-touched " files" experience-class)

      :default
      (describe-experience-to-append commit-experience))))

(defn- describe-medium-risk-commit
  [commit-experience commit-diffusion commit-churn {:keys [files-touched added-loc]}]
  (let [low-diffusion?  (< commit-diffusion 0.1)
        experience-class (str ". " (describe-experience-to-append commit-experience))
        low-churn? (< commit-churn 0.1)]
    (cond
      (and low-diffusion? low-churn?)
      (describe-experience-to-append commit-experience)

      (and (not low-diffusion?) low-churn?)
      (str "The change is medium risk as it's more diffused ("
           files-touched " files modifified) than your typical change set"  experience-class)

      (and low-diffusion? (not low-churn?))
      (str "The change is medium risk as it is a more wide-spread change ("
           added-loc " lines of code in " files-touched " files) than your typical change patterns" experience-class)

      (and (not low-diffusion?) (not low-churn?))
      (str "The change is medium risk and modifies "
           added-loc " lines of code in " files-touched " files, which is more than your typical change set" experience-class)

      :default
      (describe-experience-to-append commit-experience))))

(defn- describe-high-risk-commit
  [commit-experience commit-diffusion commit-churn {:keys [files-touched added-loc]}]
  (let [low-diffusion?  (< commit-diffusion 0.1)
        experience-class (str ". " (describe-experience-to-append commit-experience))
        low-churn? (< commit-churn 0.1)]
    (cond
      (and low-diffusion? low-churn?)
      (describe-experience-to-append commit-experience)

      (and (not low-diffusion?) low-churn?)
      (str "The change is high risk as it more diffused (" files-touched " files modified) than your normal change sets" experience-class)

      (and low-diffusion? (not low-churn?))
      (str "The change is high risk as it is more wide-spread than your typical change sets, touching " added-loc " lines of code" experience-class)

      (and (not low-diffusion?) (not low-churn?))
      (str "The change is high risk and modifies " added-loc " lines of code in " files-touched " files" experience-class)

      :default
      (describe-experience-to-append commit-experience))))

(defn- describe-risk-for-humans
  [risk-value commit-experience commit-diffusion commit-churn commit-stats]
  "All predictors are normalized values, 0->1.0. This means we can
   categorize them using hardcoded thresholds."
  (cond
      (< risk-value 5)
      (describe-low-risk-commit commit-experience commit-diffusion commit-churn commit-stats)

      (< risk-value 7)
      (describe-medium-risk-commit commit-experience commit-diffusion commit-churn commit-stats)

      :default
      (describe-high-risk-commit commit-experience commit-diffusion commit-churn commit-stats)))

; Extracted into its own public function since we want to re-use it for the
; branch predictions.
(defn commit-risk-with-known-predictors
  [author-experience-lookup diffusions parsed-commits]
  (let [commit-experience (normalize-author-experience author-experience-lookup parsed-commits)
        commit-diffusion (normalize-file-diffusion diffusions parsed-commits)
        commit-churn (normalize-churn diffusions parsed-commits)
        added-loc (loc-added-in parsed-commits)
        files-touched (distinct-entities-in parsed-commits)
        risk-value (risk-of-commits author-experience-lookup
                                    diffusions
                                    {:commit-diffusion commit-diffusion
                                     :commit-experience commit-experience
                                     :commit-churn commit-churn})
        description (describe-risk-for-humans risk-value
                                              commit-experience commit-diffusion commit-churn
                                              {:added-loc added-loc
                                               :files-touched files-touched})]
    {:risk-value risk-value
    :description description}))

(defn commit-risk
  "Performs a delta analysis of the given commits relative
   to the analysis results given by the analysis-path-fn (typically
   the last full analysis).
   NOTE that we expect the repo to be updated _before_ this function
   is invoked - otherwise the commits may NOT be present in the
   local copy."
  [analysis-path-fn repo parsed-commits]
  (if (commits-of-interest? parsed-commits)
    (let [author-stats-file (analysis-path-fn paths/author-churn-csv)
          author-experience-lookup (->> author-stats-file shared/read-csv-sans-header-from predictors/author-experience-lookup)
          diffusion-file (analysis-path-fn paths/diffusion-by-revision-csv)
          diffusions (->> diffusion-file shared/read-csv-sans-header-from (diffusion-for-repository repo))]
      (commit-risk-with-known-predictors author-experience-lookup diffusions parsed-commits))
    ;; `parsed-commits` are really "file revisions", i.e. file paths
    ;; touched by the commits in a delta analysis that are not excluded.
    ;; If `parsed-commits` is empty, we cannot calculate the average author
    ;; experience of the commit set, and thus we return risk 1 (the lowest
    ;; possible risk value).
    (do
      (log/info "Delta analysis run on empty sequence of file revisions, causing risk to default to 1. This might be caused by an exclusion filter.")
      {:risk-value 1
      :description "All commits are filtered away in this change set due to your CodeScene settings"})))
