(ns codescene.analysis.visualization.shared-aspects)

(defn safe-seq-max
  [vs]
  (if (seq vs)
    (apply max vs)
    1))

(defn has-defect-density-info?
  [densities]
  (not (empty? densities)))

(defn defect-density-aspect-from
  [defects]
  {:id "defects"
   :name "Defects"
   :maxValue (->> defects vals (map :defects) safe-seq-max)})

(defn hotspots-aspect-from
  [hotspots]
  {:id "hotspots"
   :name "Hotspots"
   :maxValue (->> hotspots (map second) safe-seq-max)})

(defn code-age-aspect-from
  [code-age]
  {:id "codeAge"
   :name "Code Age"
   :maxValue (->> code-age vals safe-seq-max)})

(defn code-churn-aspect-from
  [churn]
  {:id "codeChurn"
   :name "Code Churn"
   :maxValue (->> churn vals safe-seq-max)})

(def ^:const knowledge-loss-aspect
  {:id "loss"
   :name "Knowledge Loss"
   :maxValue 0})

(def ^:const fragmentation-aspect
  {:id "fragmentation"
   :name "Coordination Needs"
   :maxValue 1.0})

(def ^:const technical-sprawl-aspect
  {:id "sprawl"
   :name "Technical Sprawl"
   :maxValue 0})
