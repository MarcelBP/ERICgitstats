(ns codescene.configuration.exclusion-filters
  (:require
    [evolutionary-metrics.core :as evo-specs]
    [evolutionary-metrics.mining.vcs :as vcs]
    [evolutionary-metrics.mining.specs :as mining-specs]
    [codescene.analysis.specs :as analysis-specs]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]))



(def ^{:private true} possible-exclusions
  #{
    ;; generic file types
    "json"
    "md"
    "xml"
    "xsd"
    "html"
    "htm"
    "txt"
    "csv"
    "css"
    "scss"
    "less"
    "MF"
    "yml"
    "yaml"
    "config"
    "lock"

    ;; .NET
    "Designer.cs"
    "xproj"
    "vbproj"
    "csproj"
    "resx"
    "properties"
    "il"
    "sln"

    "cmake"

    ;; C++ C
    "h"
    "hpp"

    ;; gradle
    "gradle"
    "targets"
    "mk"
    "sh"
    "checks"})

(def ^{:private true} exclusion-re
  (re-pattern
    (str "\\.("  
      (string/join "|" possible-exclusions)
      ")$")))


(defn match-existing-extensions
  [paths]
  (->> paths
       (map #(re-find exclusion-re %))
       (filter identity)
       (map (comp (partial str "*") first))
       set))

(s/fdef match-existing-extensions
  :args (s/cat :paths (s/coll-of ::analysis-specs/filename))
  :ret (s/coll-of ::analysis-specs/file-extensions-pattern :kind set?))

(defn suggest-file-exclusions
  [git-cmd repository-paths]
  (->> repository-paths
    (mapcat
      (fn [repo]
        (second
          ;; in this case, always handle non-ASCII chars in path names
          (vcs/repository-content git-cmd true repo))))
    match-existing-extensions))

(s/def ::not-empty-string (s/and string? #(pos? (count %))))
(s/def ::git-command ::not-empty-string)


(s/fdef suggest-file-exclusions
  :args (s/and (s/cat
                 :git-cmd ::mining-specs/git-exe-command
                 :repository-paths (s/coll-of ::evo-specs/existing-path))))
