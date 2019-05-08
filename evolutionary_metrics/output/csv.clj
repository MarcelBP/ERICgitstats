(ns evolutionary-metrics.output.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [evolutionary-metrics.core :as core]
            [clojure.spec.alpha :as s]))

(defn write-to
  "Writes the given dataset ds as CSV to the given stream s.
   By default, all rows are written. This behavior
   is possible to override by providing a third argument
   specifying the number of rows to write."
  ([_s {:keys [analysis-result-header analysis-result-values]}]
     (csv/write-csv *out* [(map name analysis-result-header)])
     (csv/write-csv *out* analysis-result-values))
  ([s {:keys [analysis-result-values] :as result} n-rows]
     (write-to s (assoc result :analysis-result-values (take n-rows analysis-result-values)))))

(s/fdef write-to-file
        :args (s/cat :file-name ::core/filename
                     :s (comp not nil?)
                     :result ::core/analysis-result)
        :ret any?)

(defn write-to-file
  [file-name s result]
  (with-open [out-file (io/writer file-name)]
    (binding [*out* out-file]
      (write-to s result))))
