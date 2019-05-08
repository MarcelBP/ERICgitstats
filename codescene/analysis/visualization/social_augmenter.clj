(ns codescene.analysis.visualization.social-augmenter
  (:require [codescene.analysis.visualization.default-values :as default-values]))

(defn add-analysis-results
  [{:keys [main-devs knowledge-loss fragmentation]} metadata parts file-name]
  (let [[module revs size] metadata
        owner (get main-devs module default-values/ownership)
        {:keys [loss inconclusive]} (get knowledge-loss module default-values/knowledge-loss)
        {:keys [fractal authors]} (get fragmentation module default-values/fragmentation)
        spot {:revs revs :size size :path parts :name file-name}]
    (-> spot
        (merge owner)
        (assoc :loss loss)
        (assoc :lossinconclusive inconclusive)
        (assoc :fragmentation fractal)
        (assoc :nauthors authors))))
