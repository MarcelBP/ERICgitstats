(ns codescene.analysis.architecture.aggregator
  (:require [evolutionary-metrics.trends.complexity-trend :as complexity]
            [semantic-csv.core :as sc]
            [evolutionary-metrics.app.architectural-mapper :as arch-mapper]
            [codescene.analysis.specs :as shared-specs]
            [evolutionary-metrics.core :as evo-specs]
            [clojure.spec.alpha :as s]))

(defn make-file->component-mapper
  [transformations]
  (let [mappers (->> transformations (map (juxt :pattern :transformation)) (map arch-mapper/as-group-specification))]
    (fn [file-name]
      (if-let [component-name (arch-mapper/entity->architectural-name file-name mappers)]
        component-name
        ::no-mapping))))

(defn umapped-files-in
  [gs]
  (get gs ::no-mapping))

(defn remove-unmapped-files
  [gs]
  (dissoc gs ::no-mapping))

(s/def ::filenames (s/nilable (s/coll-of ::shared-specs/filename)))

(s/def ::unmapped-files (s/and qualified-keyword? (partial = ::no-mapping)))

(s/def ::files-by-components (s/map-of (s/or :has-transformation ::shared-specs/transformation
                                             :no-transformation  ::unmapped-files)
                                       ::filenames))

(s/fdef files-by
        :args (s/cat :transformations ::shared-specs/architectural-transformations
                     :file-names ::filenames)
        :ret ::files-by-components)

(defn files-by
  [transformations file-names]
  {:post [(s/valid? ::files-by-components %)]}
  (-> transformations
      make-file->component-mapper
      (group-by file-names)))

(s/fdef files-by-component
        :args (s/cat :hotspots-log-path ::evo-specs/existing-path
                     :file-name-fn  keyword?
                     :transformations ::shared-specs/architectural-transformations)
        :ret ::files-by-components)

(defn files-by-component
  [hotspots-log-path file-name-fn transformations]
  (->> hotspots-log-path
       sc/slurp-csv
       (map file-name-fn)
       (filter complexity/language-rules-for) ; introduced to calculate trends for deleted content where we may get all kinds of noise...
       (files-by transformations)))
