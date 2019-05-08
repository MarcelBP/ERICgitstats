(ns codescene.analysis.visualization.file-level-hotspots
  (:require [clojure.data.json :as json]
            [codescene.analysis.visualization.enclosure-visualizer :as visualizer]
            [codescene.analysis.visualization.parse-analysis-results :as parse]
            [codescene.analysis.visualization.specs :as visualization-specs]
            [codescene.analysis.visualization.shared-aspects :as aspects]
            [codescene.analysis.visualization.default-values :as default-values]
            [codescene.analysis.specs :as shared-specs]
            [codescene.analysis.architecture.technical-sprawl :as technical-sprawl]
            [hotspots-x-ray.recommendations.code-health-markers :as code-markers]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]))

(def ^:private unprioritized-hotspot-value 2)

(defn- add-density-if-exists
  [node module {:keys [defect-density]}]
  (if-let[d (get defect-density module)]
    (-> node
        (assoc :defectdensity (:density d))
        (assoc :ndefects (:defects d)))
    node))

(defn- augment-spot-with-analysis-results
  [{:keys [main-devs code-age knowledge-loss churn prioritized-hotspots code-marker-scores] :as results} metadata parts file-name]
  (let [[module revs size] metadata
        owner (get main-devs module default-values/ownership)
        age (get code-age module 0)
        {:keys [loss inconclusive]} (get knowledge-loss module default-values/knowledge-loss)
        churn-val (get churn module 0)
        language (technical-sprawl/language-name file-name)
        classification (get prioritized-hotspots module unprioritized-hotspot-value)
        code-score (get code-marker-scores module code-markers/no-score)
        spot {:revs revs :size size :path parts :name file-name}]
    (-> spot
        (assoc :age age)
        (merge owner)
        (assoc :loss loss)
        (assoc :lossinconclusive inconclusive)
        (assoc :churn churn-val)
        (assoc :language language)
        (assoc :classification classification)
        (assoc :score code-score)
        (add-density-if-exists module results))))

(s/def ::classification #{0 1 2})
(s/def ::prioritized-hotspots (s/map-of ::shared-specs/filename ::classification))

(s/def ::density nat-int?)
(s/def ::defects nat-int?)
(s/def ::density-info (s/keys :req-un [::density ::defects]))
(s/def ::defect-density (s/map-of ::shared-specs/filename ::density-info))

(s/def ::lookupable-analysis-results
  (s/keys :req-un [::visualization-specs/hotspots
                   ::visualization-specs/main-devs
                   ::visualization-specs/code-age
                   ::visualization-specs/knowledge-loss
                   ::visualization-specs/churn
                   ::prioritized-hotspots
                   ::defect-density]))

(s/fdef hotspots-format
        :args (s/cat :analysis-results ::lookupable-analysis-results
                     :root-name string?)
        :ret map?)

(defn hotspots-format
  [{:keys [hotspots] :as analysis-results} root-name]
  (let [hotspot-file-presenter (partial augment-spot-with-analysis-results analysis-results)]
    (visualizer/enclosure-format root-name hotspot-file-presenter hotspots)))

(def ^:private prioritized-hotspots-aspect
  {:id "refactoringtargets"
   :name "Refactoring Targets"
   :maxValue 2})

(def ^:private programming-language-aspect
  {:id "language"
   :name "Programming Language"
   :maxValue 0})

(defn- churn-or-defects-from
  "Not all codebases have information to calulcate defect density
   based on commit messages. If the defect info isn't there, we fall
   back to code churn since that's the legacy info we used to present.
   That is, if a user doesn't configure a defect pattern, then CodeScene
   will look as it did before we introduced the Defect Density aspect."
  [churn defect-densities]
  (if (aspects/has-defect-density-info? defect-densities)
    (aspects/defect-density-aspect-from defect-densities)
    (aspects/code-churn-aspect-from churn)))

(s/def ::json-structure map?)

(s/fdef enclosure-json-ready
        :args (s/cat :analysis-results ::lookupable-analysis-results
                     :root-name string?)
        :ret ::json-structure)

(defn enclosure-json-ready
  [{:keys [hotspots code-age churn defect-density] :as lookupable-analysis-results} root-name]
  (let [enclosure (hotspots-format lookupable-analysis-results root-name)
        enclosure-root {:version 1
                        :aspects [(aspects/hotspots-aspect-from hotspots)
                                  prioritized-hotspots-aspect
                                  (aspects/code-age-aspect-from code-age)
                                  (churn-or-defects-from churn defect-density)
                                  programming-language-aspect]}]
    (merge enclosure-root enclosure)))


(defn write-enclosure-graph
  [project
   hotspots-file main-devs-file code-age-file knowledge-loss-file churn-file prioritized-hotspots-file code-bio-marker-scores-file
   defect-density-by-file
   destination]
  (let [spots (parse/hotspots project hotspots-file)
        main-devs (parse/knowledge main-devs-file)
        code-age (parse/code-age code-age-file)
        knowledge-loss (parse/knowledge-loss knowledge-loss-file)
        churn (parse/churn churn-file)
        prioritized-hotspots (parse/prioritized-hotspots prioritized-hotspots-file)
        code-markers (parse/code-bio-markers code-bio-marker-scores-file)
        defect-density (parse/defect-density defect-density-by-file)
        result (enclosure-json-ready {:hotspots spots
                                      :main-devs main-devs
                                      :code-age code-age
                                      :knowledge-loss knowledge-loss
                                      :churn churn
                                      :prioritized-hotspots prioritized-hotspots
                                      :code-marker-scores code-markers
                                      :defect-density defect-density}
                                     "System")] ; TODO: replace with project name!
    (with-open [out-file (io/writer destination)]
      (json/write result out-file))))

