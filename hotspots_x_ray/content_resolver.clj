(ns hotspots-x-ray.content-resolver
  (:require [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [clojure.java.io :as io])
  (:import (java.io IOException)))

(defn as-child-path
  [parent child]
  (.getPath (io/file parent child)))

(defn- path-exists?
  [path]
  (some-> path io/as-file .exists))

(defn- throw-when-invalid
  [repository-path]
  (when-not (path-exists? repository-path)
    (throw (IOException. (str "Cannot access the repository at '" repository-path "'")))))

(defn file-in-project-repository-paths-from
  [repository-paths file-name]
  (when (empty? repository-paths)
    (throw (ex-info "No repository paths supplied, impossible to locate file in project"
                    {:file-name file-name})))
  (let [[scope real-file-name] (file-patterns/de-scope file-name)
        scope->repo-lookup (file-patterns/make-scope->repo-lookup repository-paths)
        repository-path (scope->repo-lookup scope)]
    (throw-when-invalid repository-path)
    [repository-path real-file-name]))
