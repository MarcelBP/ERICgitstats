(ns evolutionary-metrics.analysis.logical-coupling
  (:require [evolutionary-metrics.analysis.coupling-algos :as c]
            [evolutionary-metrics.analysis.math :as m]
            [clojure.math.numeric-tower :as math]))

;;; This module calculates the logical coupling of all modules.
;;;
;;; Logical coupling refers to modules that tend to change together.
;;; It's information that's recorded in our version-control systems (VCS).
;;;
;;; Input: all analysis expect an Incanter dataset with (at least) the following columns:
;;; :entity :rev
;;;
;;; Oputput: the analysis returns an Incanter dataset with the following columns:
;;; :entity :coupled :degree :average-revs

(defn- as-logical-coupling-measure
  "This is where the result is assembled.
   We already have all the data. Now we just pass through the
   coupled modules, with their co-change frequencies, and
   transform it to a degree of coupling.
   The coupling formula is simple: the number of shared
   revisions divided by the average number of revisions for
   the two coupled modules."
  [ds options within-threshold-fn?]
  (let [co-changing (c/co-changing-by-revision ds options)
        module-revs (c/module-by-revs co-changing)
        coupling (c/coupling-frequencies co-changing)]
    (for [[[first-entity second-entity] shared-revs] coupling
          :let [average-revs (m/average (module-revs first-entity) (module-revs second-entity))
                coupling (m/as-percentage (/ shared-revs average-revs))]
          :when (within-threshold-fn? average-revs shared-revs coupling)]
      {:entity first-entity
       :coupled second-entity
       :degree (int coupling)
       :average-revs (math/ceil average-revs)})))

(defn by-degree
  "Calculates the degree of logical coupling. Returns a seq
   sorted in descending order (default) or an optional, custom sorting criterion.
   The calulcation is  based on the given coupling statistics.
   The coupling is calculated as a percentage value based on
   the number of shared commits between coupled entities divided
   by the average number of total commits for the coupled entities."
  [ds options]
  (let [vs (->>
             (partial c/within-threshold? options)
             (as-logical-coupling-measure ds options)
             (sort-by (juxt :degree :average-revs))
             (map (juxt :entity :coupled :degree :average-revs))
             reverse)]
    {:analysis-result-header [:entity :coupled :degree :average-revs]
     :analysis-result-values vs}))
