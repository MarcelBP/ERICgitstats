(ns codescene.analysis.closed-maat-proxy
  (:require [evolutionary-metrics.app.app :as maat]
            [taoensso.timbre :as log]))

;; This module encapsulates the invokation of closed-maat (our analysis engine).

(defn run-closed-maat
  [log-file analysis dest-file analysis-options]
  (let [fixed {:log log-file
               :version-control "id"
               :analysis analysis
               :outfile dest-file}
        with-user-options (merge fixed analysis-options)]
    (log/trace "Running analysis engine on <" log-file "> with options = " with-user-options)
    (maat/run log-file with-user-options)))

(defn project->closed-maat-parameters
  [{:keys [minrevs minsharedrevs mincoupling maxcoupling maxchangesetsize cut-off-age-for-active-author]}]
  {:min-revs minrevs
   :min-shared-revs minsharedrevs
   :min-coupling mincoupling
   :max-coupling maxcoupling
   :max-changeset-size maxchangesetsize
   :cut-off-age-for-active-author cut-off-age-for-active-author})
