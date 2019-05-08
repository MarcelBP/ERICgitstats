(ns codescene.analysis.visualization.cost-hotspots
  (:require [clojure.data.json :as json]
            [codescene.analysis.visualization.enclosure-visualizer :as visualizer]
            [codescene.analysis.visualization.parse-analysis-results :as parse]
            [codescene.analysis.visualization.specs :as visualization-specs]
            [codescene.analysis.visualization.shared-aspects :as aspects]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]))

(defn- augment-spot-with-analysis-results
  [{:keys [main-devs]} metadata parts file-name]
  (let [[module revs size] metadata
        owner (get main-devs module "-")
        spot {:revs revs :size size :path parts :name file-name}]
    (assoc spot :owner owner)))

(s/def ::lookupable-analysis-results
  (s/keys :req-un [::visualization-specs/hotspots
                   ::visualization-specs/main-devs]))

(s/fdef hotspots-format
        :args (s/cat :analysis-results ::lookupable-analysis-results
                     :root-name string?)
        :ret map?)

(defn hotspots-format
  [{:keys [hotspots] :as analysis-results} root-name]
  (let [hotspot-file-presenter (partial augment-spot-with-analysis-results analysis-results)]
    (visualizer/enclosure-format root-name hotspot-file-presenter hotspots)))

(defn- costspot-aspect-from
  [hotspots]
  {:id "costspots"
   :name "Costs"
   :maxValue (->> hotspots (map second) aspects/safe-seq-max)})

(s/def ::json-structure map?)

(s/fdef enclosure-json-ready
        :args (s/cat :analysis-results ::lookupable-analysis-results
                     :root-name string?)
        :ret ::json-structure)

(defn enclosure-json-ready
  [{:keys [hotspots] :as lookupable-analysis-results} root-name]
  (let [enclosure (hotspots-format lookupable-analysis-results root-name)
        enclosure-root {:version 1
                        :aspects [(costspot-aspect-from hotspots)]}]
    (merge enclosure-root enclosure)))

(defn write-enclosure-graph
  [project hotspots-file main-devs-file destination]
  (let [spots (parse/hotspots project hotspots-file)
        main-devs (parse/knowledge main-devs-file)
        result (enclosure-json-ready {:hotspots spots
                                      :main-devs main-devs}
                                     "System")] ; TODO: replace with project name!
    (with-open [out-file (io/writer destination)]
      (json/write result out-file))))



