(ns codescene.biomarkers.file-level-hotspots
  (:require [codescene.biomarkers.code-markers-analysis :as code-markers-analysis]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [semantic-csv.core :as sc]
            [codescene.biomarkers.candidate-selection :as candidate-selection]
            [codescene.note-watch.utils :as nu]))

;; This module serves as entry point for the file level biomarker calculations.
;; That is, we operate on a system-wide level to calculate biomarkers for the
;; identified hotspots.
;;
;; The reason for separating this out is because we introduce biomarkers on an
;; architectural level too, and I look for a clean separation between them; The
;; biomarker calculations are identical, but the input filtering will be quite
;; different.
;;

(defn- prioritized-hotspot-paths
  "We need biomarkers for all priorized (ML algorithm) hotspots."
  [hotspot-file]
  (->> (sc/slurp-csv hotspot-file)
       (map :name)))

(defn- candidate-hotspot-paths
  "We want to present a stable and predictable set of biomarkers on the
   head of the power law distribution of change frequencies."
  [hotspot-file repo-paths notes-risks]
  (-> (sc/slurp-csv hotspot-file)
      (code-markers-analysis/candidate-hotspots-from repo-paths notes-risks)))

(defn write-scores
  "Generates two files, one CSV with the raw scores intended for hotspot augmentation, and
   one JSON file with all the code properties. This second JSON file is intended for
   detailed recommendations (e.g. 'increase the cohesion..')."
  [project context prioritized-hotspots-file hotspots-csv-file notes-risks-file scores-output detailed-scores-output]
  (let [notes-risks (nu/read-notes-risks notes-risks-file)
        candidate-spots (candidate-hotspot-paths hotspots-csv-file (:repo-paths project) notes-risks)
        prioritized-spots (prioritized-hotspot-paths prioritized-hotspots-file)
        {:keys [file-by-score details]} (code-markers-analysis/detailed-biomarkers-for project
                                                                                       context
                                                                                       code-markers-analysis/no-cached-scores
                                                                                       notes-risks
                                                                                       candidate-spots
                                                                                       prioritized-spots)]
    (sc/spit-csv scores-output file-by-score)
    (with-open [out-details (io/writer detailed-scores-output)]
      (json/write details out-details))))
