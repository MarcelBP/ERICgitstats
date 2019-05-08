(ns evolutionary-metrics.complexity.architectural-loco
  (:require [evolutionary-metrics.app.architectural-mapper :as architectural-mapper]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [evolutionary-metrics.mergers.shared-mergers :as shared]))

(def ^:private file-name-from second)

(defn- file-name->architectural-name
  [group-spec [language filename blanks comments code]]
  (let [n (architectural-mapper/entity->architectural-name filename group-spec)]
    [language n (Integer/parseInt blanks) (Integer/parseInt comments) (Integer/parseInt code)]))

(defn- reduce-component-group
  [group]
  (reduce (fn [[_language _component acc-blanks acc-comments acc-code] [language component blanks comments code]]
            [language component (+ acc-blanks blanks) (+ acc-comments comments) (+ acc-code code)])
          group))

(defn- by-component
  [stats-by-file group-spec]
  (->> stats-by-file
       (filter (comp (partial architectural-mapper/within-group? group-spec) file-name-from))
       (map (partial file-name->architectural-name group-spec))
       (group-by file-name-from)
       (into [])
       (map (comp reduce-component-group second))))

(defn- aggregate-by-architectural-components
  [transformations-file-name stats]
  (->> transformations-file-name
       architectural-mapper/text->group-specification
       (by-component stats)))

(defn detailed-stats-by-components
  ([detailed-result-file transformations-file-name]
   (let [detailed (shared/read-csv-sans-header-from detailed-result-file)
         component-details (aggregate-by-architectural-components transformations-file-name detailed)]
     (csv/write-csv *out* [["language" "files" "blank" "comment" "code"]])
     (csv/write-csv *out* component-details)))
  ([detailed-result-file transformations-file-name out-file-name]
   (with-open [out-file (io/writer out-file-name)]
     (binding [*out* out-file]
       (detailed-stats-by-components detailed-result-file transformations-file-name)))))
