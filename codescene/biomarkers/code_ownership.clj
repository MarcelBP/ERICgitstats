(ns codescene.biomarkers.code-ownership
  (:require [semantic-csv.core :as sc]
            [codescene.analysis.visualization.parse-analysis-results :as parse]
            [codescene.analysis.knowledge-visualizer :as knowledge-visualizer]))


(defn no-code-ownership
  "Intended as a stub in the case where we aren't interested in detecting
   code ownerships. This could be for samples that a far into the past (last year) or
   maybe some test cases."
  [_source-file]
  {})

(defn- owner-is-ex-devs? [knowledge-loss-entry]
  (= (:owner knowledge-loss-entry) knowledge-visualizer/consider-ex-devs-a-team))

(defn- get-knowledge-loss [knowledge-loss-entry]
  (if (owner-is-ex-devs? knowledge-loss-entry)
     (get knowledge-loss-entry :ownership 0.0)
     0.0))

(defn code-ownership-from
  [main-dev-file knowledge-loss-file]
  (let [main-devs (parse/knowledge main-dev-file)
        knowledge-loss-entries (parse/knowledge knowledge-loss-file)]
    (fn [source-file]
      (let [{:keys [owner ownership]} (get main-devs source-file {:owner "[unknown]" :ownership 0.0})
            knowledge-loss-entry (get knowledge-loss-entries source-file {})
            knowledge-loss (get-knowledge-loss knowledge-loss-entry)]
        {:social {:owner owner :ownership ownership :knowledge-loss knowledge-loss}}))))
