(ns codescene.analysis.visualization.architectural-hotspots
  (:require [clojure.data.json :as json]
            [codescene.analysis.visualization.enclosure-visualizer :as visualizer]
            [codescene.analysis.visualization.parse-analysis-results :as parse]
            [codescene.analysis.visualization.specs :as visualization-specs]
            [codescene.analysis.visualization.shared-aspects :as aspects]
            [codescene.analysis.visualization.default-values :as default-values]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]))

(defn- augment-spot-with-analysis-results
  [{:keys [main-devs code-age knowledge-loss churn technical-sprawl]} metadata parts file-name]
  (let [[module revs size] metadata
        owner (get main-devs module default-values/ownership)
        age (get code-age module 0)
        {:keys [loss inconclusive]} (get knowledge-loss module default-values/knowledge-loss)
        churn-val (get churn module 0)
        language (get technical-sprawl module default-values/language)
        spot {:revs revs :size size :path parts :name file-name}]
    (-> spot
        (assoc :age age)
        (merge owner)
        (assoc :loss loss)
        (assoc :lossinconclusive inconclusive)
        (assoc :churn churn-val)
        (assoc :language language))))

(s/def ::lookupable-analysis-results
  (s/keys :req-un [::visualization-specs/hotspots
                   ::visualization-specs/main-devs
                   ::visualization-specs/code-age
                   ::visualization-specs/knowledge-loss
                   ::visualization-specs/churn
                   ::visualization-specs/technical-sprawl]))

(s/fdef hotspots-format
        :args (s/cat :analysis-results ::lookupable-analysis-results
                     :root-name string?)
        :ret map?)

(defn hotspots-format
  [{:keys [hotspots] :as analysis-results} root-name]
  (let [hotspot-file-presenter (partial augment-spot-with-analysis-results analysis-results)]
    (visualizer/enclosure-format root-name hotspot-file-presenter hotspots)))

(s/def ::json-structure map?)

(s/fdef enclosure-json-ready
        :args (s/cat :analysis-results ::lookupable-analysis-results
                     :root-name string?)
        :ret ::json-structure)

(defn enclosure-json-ready
  [{:keys [hotspots code-age churn] :as lookupable-analysis-results} root-name]
  (let [enclosure (hotspots-format lookupable-analysis-results root-name)
        enclosure-root {:version 1
                        :aspects [(aspects/hotspots-aspect-from hotspots)
                                  (aspects/code-age-aspect-from code-age)
                                  (aspects/code-churn-aspect-from churn)]}]
    (merge enclosure-root enclosure)))


(defn write-enclosure-graph
  [project hotspots-file main-devs-file code-age-file knowledge-loss-file churn-file technical-sprawl-file destination]
  (let [spots (parse/hotspots project hotspots-file)
        main-devs (parse/knowledge main-devs-file)
        code-age (parse/code-age code-age-file)
        knowledge-loss (parse/knowledge-loss knowledge-loss-file)
        churn (parse/churn churn-file)
        sprawl (parse/sprawl technical-sprawl-file)
        result (enclosure-json-ready {:hotspots spots
                                      :main-devs main-devs
                                      :code-age code-age
                                      :knowledge-loss knowledge-loss
                                      :churn churn
                                      :technical-sprawl sprawl}
                                     "System")] ; TODO: replace with project name!
    (with-open [out-file (io/writer destination)]
      (json/write result out-file))))


