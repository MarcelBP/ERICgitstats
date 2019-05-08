(ns codescene.analysis.visualization.architectural-social-organization
  (:require [clojure.data.json :as json]
            [codescene.analysis.visualization.social-augmenter :as augment]
            [codescene.analysis.visualization.enclosure-visualizer :as visualizer]
            [codescene.analysis.visualization.parse-analysis-results :as parse]
            [codescene.analysis.visualization.default-values :as default-values]
            [codescene.analysis.visualization.shared-aspects :as aspect]
            [clojure.java.io :as io]))

(defn- augment-spot-with-analysis-results
  [{:keys [main-teams technical-sprawl] :as analysis-results} metadata parts file-name]
  (let [spot (augment/add-analysis-results analysis-results metadata parts file-name)
        module (first metadata)
        {:keys [owner ownership]} (get main-teams module default-values/ownership)
        team-owner {:team-owner owner :team-ownership ownership}
        language (get technical-sprawl module default-values/language)]
    (-> spot
        (merge team-owner)
        (assoc :language language))))

(defn- social-format
  [{:keys [hotspots] :as analysis-results} root-name]
  (let [hotspot-file-presenter (partial augment-spot-with-analysis-results analysis-results)]
    (visualizer/enclosure-format root-name hotspot-file-presenter hotspots)))

(def ^:private main-dev-aspect
  {:id "author"
   :name "Authors"
   :maxValue 0})

(def ^:private team-aspect
  {:id "team"
   :name "Teams"
   :maxValue 0})

(defn enclosure-json-ready
  [lookupable-analysis-results root-name]
  (let [enclosure (social-format lookupable-analysis-results root-name)
        enclosure-root {:version 1
                        :aspects [main-dev-aspect
                                  team-aspect
                                  aspect/knowledge-loss-aspect
                                  aspect/fragmentation-aspect
                                  aspect/technical-sprawl-aspect]}]
    (merge enclosure-root enclosure)))

(defn write-enclosure-graph
  [project hotspots-file main-devs-file main-teams-file knowledge-loss-file fragmentation-file technical-sprawl-file destination]
  (let [spots (parse/hotspots project hotspots-file)
        main-devs (parse/knowledge main-devs-file)
        main-teams (parse/knowledge main-teams-file)
        knowledge-loss (parse/knowledge-loss knowledge-loss-file)
        fragmentation (parse/fragmentation fragmentation-file)
        sprawl (parse/sprawl technical-sprawl-file)
        result (enclosure-json-ready {:hotspots spots
                                      :main-devs main-devs
                                      :main-teams main-teams
                                      :knowledge-loss knowledge-loss
                                      :fragmentation fragmentation
                                      :technical-sprawl sprawl}
                                     "System")] ; TODO: replace with project name!
    (with-open [out-file (io/writer destination)]
      (json/write result out-file))))



