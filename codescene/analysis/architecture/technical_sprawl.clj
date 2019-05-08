(ns codescene.analysis.architecture.technical-sprawl
  (:require [evolutionary-metrics.trends.complexity-trend :as complexity]
            [codescene.analysis.architecture.aggregator :as aggregator]
            [semantic-csv.core :as sc]
            [codescene.analysis.specs :as shared-specs]
            [clojure.spec.alpha :as s]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(s/def ::programming-language string?)
(s/def ::main-language ::programming-language)
(s/def ::technical-sprawl-by-component (s/coll-of (s/tuple ::shared-specs/transformation ::main-language)))

(s/def ::module ::shared-specs/filename)
(s/def ::code int?)

(s/def ::hotspots (s/coll-of (s/keys :req-un [::module ::code])))

(defn language-name
  [f]
  (-> f complexity/language-rules-for :language))

(defn main-language
  [files-with-code-size fs]
  (->> fs
       (map (fn [f] {:language (language-name f) :size (get files-with-code-size f 0)}))
       (group-by :language)
       (map (fn [[lang stats]] [lang (->> stats (map :size) (reduce +))]))
       (sort-by second >)
       (map first) ; only languages left
       first)) ; pick the top language

(defn- main-language-of
  [files-with-code-size components-with-files]
  (map (fn [[n fs]] {:name n :language (main-language files-with-code-size fs)}) components-with-files))

(s/fdef by-main-language-of-component
        :args (s/cat :transformations ::shared-specs/architectural-transformations
                     :hotspots ::hotspots)
        :ret ::technical-sprawl-by-component)

(defn by-main-language-of-component
  [transformations hotspots]
  (let [files-with-code-size (->> hotspots (map (juxt :module :code)) (into {}))
        files-by-components (aggregator/remove-unmapped-files (aggregator/files-by transformations (keys files-with-code-size)))]
    (->> files-by-components
         (main-language-of files-with-code-size)
         (map (juxt :name :language)))))

(defn- run-when-definitions-exist
  [{:keys [architectural-transformations] :as _context} hotspot-file]
  (if (seq architectural-transformations)
    (let [hotspots (->> hotspot-file sc/slurp-csv (sc/cast-with {:code sc/->int}))]
      (by-main-language-of-component architectural-transformations hotspots))
    []))

(defn by-main-language-of-component-to-disk
  [context hotspot-file destination]
  (let [sprawl (run-when-definitions-exist context hotspot-file)]
    (with-open [out-file (io/writer destination)]
      (csv/write-csv out-file [["component" "language"]])
      (csv/write-csv out-file sprawl))))
