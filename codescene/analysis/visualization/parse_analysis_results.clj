(ns codescene.analysis.visualization.parse-analysis-results
  (:require [semantic-csv.core :as sc]
            [codescene.analysis.visualization.shared-aspects :as shared]
            [codescene.analysis.knowledge-visualizer :as knowledge-visualizer]
            [clojure.java.io :as io]))


(defn knowledge
  [f]
  (->> f
       sc/slurp-csv
       (sc/cast-with {:ownership sc/->double})
       (map (fn [{:keys [entity main-dev ownership]}] [entity {:owner main-dev :ownership ownership}]))
       (into {})))

(defn- loss->to-indexable-tuple
  [{:keys [entity] :as _loss} resolved-loss]
  [entity resolved-loss])

(defn ownership-group->loss
  [{:keys [main-dev ownership] :as loss}]
  (loss->to-indexable-tuple loss
                            (cond

                              (= main-dev knowledge-visualizer/active-developers-team)
                              {:loss (float (- 1.0 ownership))
                               :inconclusive false}

                              (= main-dev knowledge-visualizer/consider-ex-devs-a-team)
                              {:loss ownership
                               :inconclusive false}

                              :else
                              {:loss ownership ; this happens if an author is excluded from the analysis
                               :inconclusive true})))

(defn hotspots-matching-filter
  [{:keys [minfilesize minrelativerevs] :or {minfilesize 0 minrelativerevs 0}} spots]
  (let [parsed-spots (sc/cast-with {:revisions sc/->int :code sc/->int} spots)
        normalized-cutoff-point (/ minrelativerevs 100.0)
        max-revisions (->> parsed-spots (map :revisions) shared/safe-seq-max float)]
    (->> spots
         (sc/cast-with {:revisions sc/->int :code sc/->int})
         (filter (comp (partial <= minfilesize) :code))
         (filter (fn [{:keys [revisions]}]
                   (<= normalized-cutoff-point (/ revisions max-revisions))))
         (map (juxt :module :revisions :code)))))

(defn hotspots
  [content-filter f]
  (->> f
       sc/slurp-csv
       (hotspots-matching-filter content-filter)))

(defn churn
  [f]
  (->> f
       sc/slurp-csv
       (sc/cast-with {:churn sc/->int})
       (map (juxt :module :churn))
       (into {})))

(defn sprawl
  [f]
  (->> f
       sc/slurp-csv
       (map (juxt :component :language))
       (into {})))

(defn knowledge-loss
  [f]
  (->> f
       sc/slurp-csv
       (sc/cast-with {:ownership sc/->double})
       (map ownership-group->loss)
       (into {})))

(defn fragmentation
  [f]
  (->> f
       sc/slurp-csv
       (map (fn [{:keys [entity fractal-value n-authors]}] [entity {:fractal fractal-value :authors n-authors}]))
       (into {})))

(defn code-age
  [f]
  (->> f
       sc/slurp-csv
       (sc/cast-with {:age-months sc/->int})
       (map (juxt :entity :age-months))
       (into {})))

(defn prioritized-hotspots
  [f]
  (->> f
       sc/slurp-csv
       (sc/cast-with {:classification sc/->int})
       (map (juxt :name :classification))
       (into {})))

(defn code-bio-markers
  [f]
  (->> f
       sc/slurp-csv
       (sc/cast-with {:score sc/->int})
       (map (juxt :name :score))
       (into {})))

(defn defect-density
  [f]
  (->> f
       sc/slurp-csv
       (sc/cast-with {:density sc/->int :defects sc/->int})
       (map (fn [{:keys [name density defects]}]
              [name {:density density :defects defects}]))
       (into {})))
