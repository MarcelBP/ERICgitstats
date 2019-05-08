(ns hotspots-x-ray.diagnostics.parse-errors
  (:require [taoensso.timbre :as timbre]))

(defn trace-potential-errors
  [file-path error-listener]
  (let [errors (.getDetectedErrors error-listener)]
    (doseq [error errors]
      (let [reason (first error)
            rule-stack (second error)]
        (timbre/tracef "Parse failure in '%s': %s, rule stack: %s" file-path reason rule-stack)))
    errors))
