(ns evo-pattern-detector.core
  (:gen-class)
  (:require [evo-pattern-detector.spots :as spots]
            [clojure.string :as string]
            [clojure.stacktrace :as stacktrace]
            [clojure.tools.cli :as cli]))

(def cli-options
  [["-i" "--identity IDENTITY" "An identity file of all commits"]
   ["-s" "--spots SPOTS" "An augmented hotspots file with input data"]
   ["-f" "--fragmentation FRAGMENTATION" "A file with fractal values as input data"]
   ["-c" "--soc SOC" "A file with Sum of Coupling as input data"]
   ["-o" "--out OUT" "Output file name where the classification results are written"]
   ["-t" "--stat STAT" "Ouptut file name where the statistic results are written"]
   ["-h" "--help"]])

(defn- usage [options-summary]
  (->> ["This a proprietary product of Empear Analytics AB used to identify patterns in evolutionary metrics."
        "Version: 0.1.0-SNAPSHOT"
        ""
        "Usage: program-name <input-files> --out <classification-result-file> --stat <statistics-result-file>"
        ""
        "Options:"
        options-summary
        "Please refer to the manual page for more information."]
       (string/join \newline)))

(defn- error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn- exit [status msg]
  (println msg)
  (System/exit status))

(defn- ensure-arg-exists
  [arg options]
  (if-let [v (get options arg)]
    v
    (throw (IllegalArgumentException. (str "You must provide a value for the option <" (name arg) ">")))))

(defn -main
  [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
     (:help options) (exit 0 (usage summary))
     errors (exit 1 (error-msg errors)))
    :else
    (try
      (spots/classify-hotspots (ensure-arg-exists :identity options)
                               (spots/hotspot-files->input-seq (ensure-arg-exists :spots options))
                               (ensure-arg-exists :fragmentation options)
                               (ensure-arg-exists :soc options)
                               (ensure-arg-exists :out options) 
                               (ensure-arg-exists :stat options)
                               {}) ; no options -> run with the real clustering algorithm
      (flush)
      (catch IllegalArgumentException e
        (println "Invalid argument: " (.getMessage e))
        (stacktrace/print-stack-trace e)
        (exit 1 (usage summary)))
      (catch Exception e ; this is our main recovery point
        (println "Error: " (.getMessage e))
        (stacktrace/print-stack-trace e)
        (exit 1 (usage summary))))))
