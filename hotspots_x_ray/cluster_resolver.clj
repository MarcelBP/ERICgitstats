(ns hotspots-x-ray.cluster-resolver
  (:require [hotspots-x-ray.recommendations.coupling :as coupling]
            [hotspots-x-ray.x-rayer :as x-rayer]
            [hotspots-x-ray.cleansing.functions :as cleanse]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [taoensso.timbre :as log]))

(defn- functions-in-revision
  [[k v]]
  (let [fs (->> v (map :entity) flatten)]
    {:changes fs :rev k}))

(defn internal-temporal-coupling-analysis-format
  [file-name evo-metrics]
  (let [{:keys [evolution current-version]} evo-metrics
        by-rev (group-by :rev evolution)
        changes (map functions-in-revision by-rev)]
    {:file-name file-name :current-function-bodies (cleanse/current-function-bodies-in current-version) :revisions (count by-rev) :changes-by-revision changes}))

(defn- resolver-format-for
  [start-date git-cmd thresholds {:keys [repo file-name]}]
  (let [evo (x-rayer/describe-evolution-of repo start-date file-name git-cmd thresholds)]
    (internal-temporal-coupling-analysis-format file-name evo)))

;; Alright, this looks much more tricky than necessary. The reason is because I don't want to break
;; any calculations, so let's do the calculations with the relative file name and then augment the
;; results before writing it to disk. This guarantees that we keep the order, which failed (for unknown reasons)
;; in the regression tests when I tried to do it properly.
(defn- add-repo-scope
  [repo [p1 p2 degree average-revs similarity]]
  [(file-patterns/add-repo-name repo p1) (file-patterns/add-repo-name repo p2) degree average-revs similarity])

(defn- all-repos-in
  [cluster]
  (->> cluster (map :repo) distinct))

(def ^:private default-repo-name "unknwon")

(defn- repo-name-from
  "Works because all files are in the same repo (guaranteed by context...)."
  [cluster]
  (if (seq cluster)
    (let [rs (all-repos-in cluster)]
      (if (= 1 (count rs))
        (->> rs first file-patterns/final-segment)
        (do
          (log/error (str "The coupling resolver by commits expects all files to be in the same repo, but that's not the case."
                          "The X-Ray continues, but you won't be able to diff the results.  Got repositories: ")
                     (pr-str rs))
          default-repo-name)))
    default-repo-name))

(defn resolve-coupling-for
  "Resolves temporal coupling for a set of files within the same repository."
  [coupling-cluster start-date git-cmd thresholds out-file-name]
  (let [internal-format (map (partial resolver-format-for start-date git-cmd thresholds) coupling-cluster)
        repo-name (repo-name-from coupling-cluster)
        couples (coupling/coupled-functions-in internal-format thresholds)
        scoped-couples (map (partial add-repo-scope repo-name) couples)]
    (with-open [out-file (io/writer out-file-name)]
      (csv/write-csv out-file [["entity" "coupled" "degree" "average-revs" "similarity"]])
        (csv/write-csv out-file scoped-couples))))