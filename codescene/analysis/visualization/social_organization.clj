(ns codescene.analysis.visualization.social-organization
  (:require [clojure.data.json :as json]
            [codescene.analysis.visualization.social-augmenter :as augment]
            [codescene.analysis.visualization.enclosure-visualizer :as visualizer]
            [codescene.analysis.visualization.shared-aspects :as aspect]
            [codescene.analysis.visualization.parse-analysis-results :as parse]
            [clojure.java.io :as io]))

(defn- social-format
  [{:keys [hotspots] :as analysis-results} root-name]
  (let [hotspot-file-presenter (partial augment/add-analysis-results analysis-results)]
    (visualizer/enclosure-format root-name hotspot-file-presenter hotspots)))

(def ^:private owner-aspect
  {:id "author"
   :name "Owners"
   :maxValue 0})

(defn enclosure-json-ready
  [lookupable-analysis-results root-name]
  (let [enclosure (social-format lookupable-analysis-results root-name)
        enclosure-root {:version 1
                        :aspects [owner-aspect
                                  aspect/knowledge-loss-aspect
                                  aspect/fragmentation-aspect]}]
    (merge enclosure-root enclosure)))

(defn write-enclosure-graph
  [project hotspots-file main-devs-file knowledge-loss-file fragmentation-file destination]
  (let [spots (parse/hotspots project hotspots-file)
        main-devs (parse/knowledge main-devs-file)
        knowledge-loss (parse/knowledge-loss knowledge-loss-file)
        fragmentation (parse/fragmentation fragmentation-file)
        result (enclosure-json-ready {:hotspots spots
                                      :main-devs main-devs
                                      :knowledge-loss knowledge-loss
                                      :fragmentation fragmentation}
                                     "System")] ; TODO: replace with project name!
    (with-open [out-file (io/writer destination)]
      (json/write result out-file))))



