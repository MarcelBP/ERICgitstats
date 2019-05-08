;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.parsers.identity
  (:require [clojure.spec.alpha :as s]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.parsers :as parsers]
            [semantic-csv.core :as sc]))

(s/fdef parse-log
        :args (s/cat :input-file-name ::core/filename
                     :options map?)
        :ret ::parsers/vcs-log)

(defn parse-log
  "Reads the given log (= the result of an identity analysis) into an
   Incanter dataset suitable for the analysis modules."
  [input-file-name _options]
  (sc/slurp-csv input-file-name))
