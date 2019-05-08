(ns codescene.delta.delta-analysis
  (:require [codescene.delta.risk-classification :as risk-classification]
            [codescene.delta.warnings :as warnings]
            [evolutionary-metrics.mining.vcs :as vcs]
            [evolutionary-metrics.parsers.git-multi-step-parser :as git-parser]
            [codescene.mining.evolution :as mining]
            [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [codescene.analysis.paths :as paths]
            [codescene.analysis.specs :as analysis-specs]
            [evolutionary-metrics.core :as evo-specs]
            [evolutionary-metrics.mining.specs :as mining-specs]
            [codescene.analysis.versioning :as versioning]
            [codescene.mining.evolution-accessor :as evolution-access]
            [clojure.java.io :as io]
            [digest :as digest]
            [clojure.spec.alpha :as s]
            [clojure.data.json :as json]
            [codescene.mining.evolution :as evolution]
            [evolutionary-metrics.app.developer-aliases :as aliases]
            [taoensso.timbre :as log])
  (:import (java.io IOException)))

(defn as-evolutionary-log
  [{:keys [exclusionfilter] :as project} git-cmd repo commits]
  (try
    (let [cs (vcs/mine-commit-range git-cmd repo commits)
          parseable (clojure.string/join "\n" cs)
          repo-scope (file-patterns/final-segment repo)
          optional-filters (mining/optional-git-mining-arguments-for project repo-scope repo git-cmd)
          complete-filter (merge optional-filters
                                 {:exclusionfilter exclusionfilter})]
      (->> (git-parser/parse-from-string parseable complete-filter)
           (remove evolution-access/binary-content?)
           (aliases/resolve-authors project)))
    (catch Exception e
      (let [reason (str "Failed to analyse the commits: " (pr-str commits) " in the repository: " repo ". Is it a valid commit in this repository?")]
        (log/error reason e)
        (throw (IllegalArgumentException. reason e))))))

(defn- delta-analysis-id-for
  [commits]
  (->> commits
       (clojure.string/join " ")
       digest/sha-1))

;;
;; Specs for the API
;;

(s/def ::existing-path (s/and string? #(.exists (io/as-file %))))

(s/def ::risk nat-int?)
(s/def ::description string?)
(s/def ::id string?)
(s/def ::category string?)
(s/def ::details (s/coll-of string?))
(s/def ::warning (s/keys :req-un [::category ::details]))

(s/def ::origin-url (s/nilable string?))
(s/def ::change-ref (s/nilable string?))

(s/def ::warnings (s/coll-of ::warning))

(s/def ::coupling-threshold-percent nat-int?)

(s/def ::use-biomarkers boolean?)

(s/def ::delta-analysis-scope (s/keys :req-un [::commits
                                               ::repo]
                                      :opt-un [::coupling-threshold-percent
                                               ::use-biomarkers
                                               ::origin-url
                                               ::change-ref]))

(s/def ::delta-analysis-result (s/keys :req-un [::risk
                                                ::description
                                                ::warnings
                                                ::id]))

;;
;; API for the actual delta analysis, free of side-effects.
;;

(s/fdef relative-to-analysis
        :args (s/cat :project ::analysis-specs/project
                     :time-now ::analysis-specs/time-now
                     :analysis-path ::evo-specs/existing-path
                     :git-cmd ::mining-specs/git-exe-command
                     :analysis-scope ::delta-analysis-scope)
        :ret ::delta-analysis-result)

(defn relative-to-analysis
  "Performs a delta analysis of the given commits relative
   to the analysis results given by the analysis-path-fn (typically
   the last full analysis).
   NOTE that we expect the repo to be updated _before_ this function
   is invoked - otherwise the commits may NOT be present in the
   local copy."
  [project time-now analysis-path git-cmd {:keys [repo commits coupling-threshold-percent use-biomarkers]}]
  (let [analysis-path-fn (partial paths/as-child-path analysis-path)
        parsed-commits (as-evolutionary-log project git-cmd repo commits)
        analysis-id (delta-analysis-id-for commits)
        version (versioning/get-analysis-version analysis-path)
        warning-context {:project project
                         :time-now time-now
                         :analysis-path analysis-path
                         :analysis-path-fn analysis-path-fn
                         :version version
                         :git-cmd git-cmd
                         :repo repo
                         :coupling-threshold-percent coupling-threshold-percent
                         :use-biomarkers use-biomarkers}
        described-risk (risk-classification/commit-risk analysis-path-fn repo parsed-commits)
        detected-warnings (warnings/detect project warning-context repo parsed-commits)]
    (merge
      {:risk     (:risk-value described-risk)
       :description (:description described-risk)
       :id       analysis-id}
      detected-warnings)))

;;
;; API with side-effects to update the repository under analysis and persiting the results.
;;

(defn- fail-with-io-exception
  [s]
  (throw (IOException. (pr-str s))))

(defn- delta-analysis-result-path
  [analysis-result-directory commits]
  (as-> commits $
        (delta-analysis-id-for $)
        (str $ ".json")
        (io/file $)
        (io/file analysis-result-directory $)
        (.getPath $)))

(defn- fetch-from-gerrit?
  [{:keys [origin-url change-ref] :as _analysis-scope}]
  (and (some? origin-url)
       (some? change-ref)))

(defn- fetch-commits-to-analyse
  [{:keys [update-repositories update-subcommand] :as _project}
   git-cmd
   {:keys [repo origin-url change-ref] :as analysis-scope}] ;fetch-changeset-from-specific-remote
  ; WARNING: special case ahead
  ; In a Gerrit setting, the staging area (i.e. the commits to analyse) is located
  ; on a separate server
  (if (fetch-from-gerrit? analysis-scope)
    (do
      (log/info "Delta analysis from staging area: updating the repo in " repo " with changes from " origin-url " for the change ref " change-ref)
      (vcs/fetch-changeset-from-specific-remote git-cmd origin-url change-ref repo)
      (log/info "Delta analysis: updated repo in " repo " with changes from " origin-url " for the change ref " change-ref))
    (evolution/update-with-fallback [repo] update-repositories (or update-subcommand :pull) false fail-with-io-exception git-cmd)))

(s/def ::result ::delta-analysis-result)
(s/def ::result-destination ::evo-specs/existing-path)

(s/def ::persisted-delta-analysis-result (s/keys :req-un [::result ::result-destination]))

(s/fdef run-delta-analysis
        :args (s/cat :project ::analysis-specs/project
                     :time-now ::analysis-specs/time-now
                     :analysis-path ::evo-specs/existing-path
                     :destination-folder ::analysis-specs/filename
                     :git-cmd ::mining-specs/git-exe-command
                     :analysis-scope ::delta-analysis-scope)
        :ret ::persisted-delta-analysis-result)

(defn run-delta-analysis
  "Runs delta analysis for given project on given repository and collection of commits.
  Optional parameters can also be passed in `analysis-scope` map - if missing the default values are used:
    :coupling-threshold-percent - minimum threshold for considering temporal couples, default 80"
  [project
   time-now
   analysis-path
   destination-folder
   git-cmd
   {:keys [commits] :as analysis-scope}]
  (fetch-commits-to-analyse project git-cmd analysis-scope)
  (let [result-destination (delta-analysis-result-path destination-folder commits)
        delta-result (relative-to-analysis project time-now analysis-path git-cmd analysis-scope)]
    (io/make-parents result-destination)
    (with-open [out-file (io/writer result-destination)]
      (json/write delta-result out-file))
    {:result delta-result
     :result-destination result-destination}))
