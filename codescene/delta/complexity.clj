(ns codescene.delta.complexity
  (:require [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [hotspots-x-ray.languages.parser :as x-ray-parser]
            [taoensso.timbre :as timbre]))

(defn- git-revision-in-analysis-for
  [analysis-repo-revisions {:keys [repo name]}]
  (if-let [r (some->> analysis-repo-revisions (filter (comp (partial = repo) :repo )) first)]
    (:revision r)
    (do
      (timbre/warn "Delta analysis: no historic revision info recorded for " repo " with file " name ". Existing: " (pr-str analysis-repo-revisions))
      "HEAD")))

(defn- files-with-master-revisions
  [scoped-files analysis-repo-revisions]
  (->> scoped-files ; TODO: wrong - this should be a map with name + delta-revision
       (map file-patterns/de-scope)
       (map (fn [[repo file]] {:repo repo :name file}))
       (map (fn [f] (assoc f :reference-revision (git-revision-in-analysis-for analysis-repo-revisions f))))))

(defn- supports-complexity-calculation
  [files]
  (filter (comp x-ray-parser/supports-file-type? :name) files))

(defn complexity-warnings-for
  [files-within-commit repos-with-main-revisions thresholds] ; TODO: specify the file + commit it occours in!
  (let [files-of-interest (supports-complexity-calculation files-within-commit)
        files-with-metadata (files-with-master-revisions files-of-interest repos-with-main-revisions)]
    []))
