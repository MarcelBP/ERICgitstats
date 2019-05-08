(ns evolutionary-metrics.parsers.line-counter
  (:require [evolutionary-metrics.mining.vcs :as vcs]
            [evolutionary-metrics.parsers :as parser-specs]
            [evolutionary-metrics.mining.specs :as mining-specs]
            [clojure.spec.alpha :as s]
            [taoensso.timbre :as log]))

(defn- binary-content?
  [v]
  (some (partial = "-") [(:loc-added v) (:loc-deleted v)]))

(s/def ::name ::parser-specs/entity)

(s/def ::code-churn (s/keys :req-un [::parser-specs/loc-added
                                     ::parser-specs/loc-deleted]))

(s/def ::revision (s/keys :req-un [::parser-specs/rev
                                   ::code-churn]))

(s/fdef lines-of-code-in
        :args (s/cat :git-cmd ::mining-specs/git-exe-command
                     :repo ::mining-specs/repository-path
                     :revision-info ::revision
                     :file-name ::parser-specs/entity)
        :ret ::code-churn)

(defn lines-of-code-in
  [git-cmd repo {:keys [rev code-churn] :as _revision-info} file-name]
  (if (binary-content? code-churn)
    code-churn
    (let [[res {:keys [error content]}] (vcs/lazy-historic-file-version git-cmd repo {:rev rev :name file-name})]
      (if res
        (let [real-loc (count content)]
          (log/debug "Adjusted LoC for split content " file-name " from " (:loc-added code-churn) " to " real-loc)
          {:loc-added (str real-loc)
           :loc-deleted "0"})
        (do
          (log/warn "Failed to resolve loc for " file-name " : " (pr-str error))
          code-churn)))))

(defn identity-code-churn
  "Used as a fast alternative to avoid the overhead of launching a Git client."
  [_git-cmd _repo {:keys [code-churn] :as _revision-info} _file-name]
  code-churn)
