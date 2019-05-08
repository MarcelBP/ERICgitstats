(ns codescene.analysis.versioning
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def ^:const current-version
  "The current version of the analysis result format. Increment this
  when introducing backwards-incompatible changes to analysis result
  formats. It must be an integer."
  7)

(def ^:const original-version
  "The original version of the analysis result format, used when the
  VERSION file is missing."
  1)

(defn get-analysis-version [analysis-directory]
  (let [version-file (io/file analysis-directory "VERSION")]
    (if (.exists version-file)
      (try
        (Integer/parseInt (string/trim (slurp version-file)))
        (catch Exception e
          (throw (ex-info
                  (str "Failed to read VERSION file from analysis results directory: " analysis-directory)
                  {:cause e}))))
      original-version)))

(defn write-analysis-version
  "Writes the analysis version file to the analysis directory, and returns the
  version. Defaults to `current-version`."
  ([analysis-directory]
   (write-analysis-version analysis-directory current-version))
  ([analysis-directory analysis-version]
   (let [version-file (io/file analysis-directory "VERSION")]
     (io/make-parents version-file)
     (spit version-file (Integer/toString analysis-version))
     analysis-version)))
