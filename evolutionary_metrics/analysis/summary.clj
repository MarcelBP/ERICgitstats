(ns evolutionary-metrics.analysis.summary
  (:require [evolutionary-metrics.analysis.authors :as authors]
            [evolutionary-metrics.analysis.entities :as entities]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.parsers :as parsers]
            [clojure.spec.alpha :as s])
  (:import (org.joda.time.base BaseSingleFieldPeriod)))

;;; This module implements a summary analysis of a given change set.
;;; The intent is to provide an overview of the data under analysis.

(s/def ::time-period #(instance? BaseSingleFieldPeriod %))

(s/def ::statistic (s/tuple string? nat-int?))
(s/def ::statistics (s/coll-of ::statistic))

(s/fdef calculate-summary
        :args (s/cat :commits ::parsers/vcs-log
                     :options map?)
        :ret ::statistics)

(defn calculate-summary
  "Calculates a summary for the data we'll analyze.
   Note that the results may differ from the ones in
   the VCS log since empty change sets (such as merges) are
   ignored in the mining."
  [commits options]
  [["Commits" (count (entities/all-revisions commits))]
   ["Entities" (count (entities/all commits))]
   ["Changed entities" (count commits)]
   ["Authors" (count (authors/all commits))]
   ["Active Authors" (count (authors/active-authors commits options))]])

(s/fdef overview
        :args (s/cat :commits ::parsers/vcs-log
                     :options map?)
        :ret ::core/analysis-result)

(defn overview
  [commits options]
  {:analysis-result-header [:statistic :value]
   :analysis-result-values (calculate-summary commits options)})
