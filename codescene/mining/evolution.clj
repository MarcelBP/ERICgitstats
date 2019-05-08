(ns codescene.mining.evolution
  (:require [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [taoensso.timbre :as log]
            [evolutionary-metrics.mining.vcs :as vcs]
            [clojure.string :as string]
            [codescene.analysis.specs :as analysis-specs]
            [evolutionary-metrics.parsers.line-counter :as line-counter]
            [clojure.spec.alpha :as s]
            [codescene.analysis.specs :as analysis-specs]))


(s/fdef update-with-fallback
        :args (s/cat
                :repo-paths ::analysis-specs/repo-paths
                :updaterepo-option boolean?
                :update-subcommand #{:pull :fetch}
                :prune-removed-remote-branches boolean?
                :warning-reporter-fn fn?
                :git-client ::analysis-specs/git-client))

(defn update-with-fallback
  "We want to fetch the latest history.
   If we fail (e.g. no network available), we'll record a warning and
   continue with the existing snapshot."
  [repo-paths updaterepo-option update-subcommand prune-removed-remote-branches warning-reporter-fn git-client]
  (when updaterepo-option
    (doseq [repo-path repo-paths]
      (log/infof "Updating the repo in %s with 'git %s' and remote branch pruning = %s" repo-path update-subcommand prune-removed-remote-branches)
      (try
        (vcs/update-git-repo git-client update-subcommand prune-removed-remote-branches repo-path)
        (catch InterruptedException interrupted
          (log/info "Repository update of <" repo-path "> interruped")
          (throw interrupted))
        (catch Exception e
          (do
            (log/warn "Failed to update the Git repo at <" repo-path ">. Reason: " e)
            (warning-reporter-fn [(str "Failed to update the " (file-patterns/final-segment repo-path) " repo before analysis")])))))))

(defn as-exclusion-options
  [{:keys [excludecontent whitelistcontent] :as _project} repo-scope]
  (merge {:parsed-entity-namespace repo-scope}
         (if (seq excludecontent)
           {:patterns-to-exclude     excludecontent}
           {})
         (if (seq whitelistcontent)
           {:patterns-to-include     whitelistcontent}
           {})))

(defn- resolve-loc-option
  [repo-path git-cmd {:keys [lookup-copied-content]}]
  (if lookup-copied-content
    {:resolve-loc-for (partial line-counter/lines-of-code-in git-cmd repo-path)
     :lookup-copied-content true}
    {:resolve-loc-for (partial line-counter/identity-code-churn git-cmd repo-path)
     :lookup-copied-content false}))

(defn optional-git-mining-arguments-for
  [project repo-scope repo-path git-cmd]
  (let [exclusions (as-exclusion-options project repo-scope)
        loc-resolved (resolve-loc-option repo-path git-cmd project)]
    (merge exclusions loc-resolved)))

(defn extensions->glob-patterns
  "The user only specifies a set of file extensions to include.
   It's our task to convert them to glob patterns that matches all
   instances of those extensions, no matter at what directory level."
  [exclusionfilter]
  {:pre [(s/valid? ::analysis-specs/exclusionfilter exclusionfilter)]}
  (if exclusionfilter
    (as-> exclusionfilter f
          (string/split f #"(^\*\.)|(;\*\.)")
          (remove empty? f)
          (map #(str "*." % ";**/*." %) f)
          (string/join ";" f))
    ""))
