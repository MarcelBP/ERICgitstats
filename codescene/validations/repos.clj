(ns codescene.validations.repos
  (:require [clojure.java.io :as io]))

(defn- duplicate-paths->str [duplicate-paths]
  (str
   "Duplicate repo names detected in the project. Please rename them to be unique:\n    "
   (->> duplicate-paths
        (map (fn [[name paths]] (str name " --> " paths)))
        (clojure.string/join " \n    "))))

(defn- assert-repo-paths-unique
  "Given repo paths checks whether the repo names are unique throwing ex-info with `:conflicts`
  key otherwise.
  Repo names must be unique otherwise the analysis fails later in the process
  with a strange exception:
   'java.lang.ClassCastException: java.lang.String cannot be cast to clojure.lang.IPersistentCollection'."
  [repo-paths]
  (when-let [duplicate-paths (->> repo-paths
                                  (group-by #(-> % (io/file) .getName))
                                  (filter (fn [[_repo-name conflicting-repo-paths]]
                                            (< 1 (count conflicting-repo-paths))))
                                  seq)]
    (throw (ex-info
            (duplicate-paths->str duplicate-paths)
            {:conflicts duplicate-paths
             :all-repo-paths repo-paths}))))

(defn valid-git-repo-at?
  [path]
  (let [f (io/file path)
        f-git (io/file path ".git")]
    (and (.exists f)
         (.isDirectory f)
         (.exists f-git))))

(defn- non-existing-repos->str
  [non-existing-repos]
  (str
   "Non existing repo paths detected in the project. Please create them:\n    "
   (clojure.string/join " \n    " non-existing-repos)))

(defn- assert-git-repos-exists
  "Given repo paths checks whether repositories exist on the disk throwing
  ex-info with `:non-existing-repo-paths` key otherwise."
  [repo-paths]
  (when-let [non-existing-repos (->> repo-paths
                                     (filter (comp not valid-git-repo-at?))
                                     seq)]
    (throw (ex-info
            (non-existing-repos->str non-existing-repos)
            {:non-existing-repo-paths non-existing-repos
             :all-repo-paths repo-paths}))))

(defn assert-analysis-constrains-fullfiled
  [repo-paths]
  (assert-repo-paths-unique repo-paths)
  (assert-git-repos-exists repo-paths))
