(ns codescene.risks.predictor-mining
  (:require [codescene.mining.cleaning :as cleaning]))

(defn- normalize-value
  [max-val [a c]]
  [a (cleaning/normalize max-val c)])

(defn- as-normalized-lookup
  [stats values-fn]
  (let [stats-of-interest (map values-fn stats)
        max-revs-contributed (->> stats-of-interest (map (comp cleaning/as-int second)) cleaning/safe-max (max 1))]
    (->> stats-of-interest
         (map (partial normalize-value max-revs-contributed))
         (into {}))))

(defn author-experience-lookup
  "Converts a seq of author,added,deleted,net,revisions,months,lastcontrib values
   into a map with author -> experience. Experience is a normalized value where
   1.0 is the author with most revisions on the project."
  [author-stats]
  (as-normalized-lookup author-stats (fn [v] [(first v) (nth v 4)])))

;; TODO: we may want to consider the use of subsystems rather
;; than files. However, that requires that the user always
;; defines architectural components _and_ that they are defined
;; on a level that's granular enough.

(defn commit-diffusion-lookup
  "Converts the results of a diffusion analysis,
   a seq of revision, date, author, n files, n sub-systems, LoC Added values, Repo
   into a map with commit -> diffusion. Diffusion is a normalized value
   where 1.0 is the commit that touches the most parts in the history of
   the codebase."
  [commit-stats]
  (as-normalized-lookup commit-stats (fn [v] [(first v) (nth v 3)])))

(defn commit-loc-added-lookup
  "Converts the results of a diffusion analysis,
   a seq of revision, date, author, n files, n sub-systems, LoC Added values, Repo
   into a map with commit -> loc-added. loc-added is a normalized value
   where 1.0 is the commit with the most code churn."
  [commit-stats]
  (as-normalized-lookup commit-stats (fn [v] [(first v) (nth v 5)])))

(defn commit-info-lookup
  "Converts the results of a diffusion analysis into a map with commit -> {author date repository}."
  [commit-stats]
  (->> commit-stats
       (map (fn [v] [(first v) {:author (nth v 2) :date (nth v 1) :repository (nth v 6)}]))
       (into {})))


