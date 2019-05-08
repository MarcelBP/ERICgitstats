(ns codescene.queries.key-personnel-detection
  (:require [semantic-csv.core :as sc]
            [incanter.stats :as stats]
            [taoensso.timbre :as log]
            [evolutionary-metrics.analysis.math :as math]
            [clojure.math.numeric-tower :as m]))

(defn- sum-locs
  [[author contribs]]
  {:author author
   :loc (->> contribs (map :author-added) (reduce +))})

(defn- loc-per-dev
  [ows]
  (->> ows
       (group-by :author)
       (map sum-locs)
       (sort-by :loc >)))

(defn- authors-at-head-of-power-law
  [power-law-cut-off-point hs]
  (let [hs-length (max (count hs) 1)]
    (loop [head hs]
      (if (empty? head) ; edge case -> handle in calling context
        head
        (let [mean (stats/mean (map :loc head))
              safe-mean (if (Double/isNaN mean) 0 mean)
              new-head (filter #(> (:loc %) safe-mean) head)
              new-head-count (count new-head)
              cut-off-point (/ new-head-count hs-length)]
          (log/debug "Detecting key personnel candidates: m = " safe-mean ", nhc = " new-head-count ", cof = " (float cut-off-point))
          (if (< cut-off-point power-law-cut-off-point)
            new-head ; we've found our candidates
            (do ; continue to search...
              (log/debug "Detecting key personnel candidates: " new-head-count " candidates left out of a total of " hs-length)
              (recur new-head))))))))

(defn- total-loc-for
  [authors]
  (->> authors (map :loc) (reduce +)))

(defn- personnel-statistics-for
  [sc total-code-size]
  (let [loc-keyp (total-loc-for sc)
        percentage-keyp (-> loc-keyp (/ total-code-size) math/as-percentage m/round)]
    (log/debug "Key Personnel: detected " (count sc) " developers with a contribution of " percentage-keyp "%.")
    {:n-key-personnel (count sc)
     :n-key-personnel-percentage percentage-keyp
     :debug-info {:total-loc total-code-size
                  :loc-keyp loc-keyp}}))

(def ^:const no-key-personnel {:n-key-personnel 0
                               :n-key-personnel-percentage 0})

(defn- total-code-size-with-ex-devs
  "When calculating a Key Personnel contribution percentage, we need to include ex-devs too.
   Otherwise we might get conflicting info like 96% by KP, 30% abandoned code.
   The numbers are correct, but inlikely to be what the user expects."
  [entity-ownership]
  (->> entity-ownership loc-per-dev total-loc-for))

(defn- pick-minimum-amount-of-personnel
  [total-code-size devs-sorted-on-loc]
  (if-let [at-least-one (first devs-sorted-on-loc)]
    (let [candidate-contrib (/ (max 1 (:loc at-least-one)) total-code-size)]
      (if (> candidate-contrib 0.2) ; no key-personnel if they have done less than 20%
        (personnel-statistics-for [at-least-one] total-code-size)
        no-key-personnel))
    no-key-personnel)) ; give-up...

(defn- statistics-for-authors-at-head-of-power-law
  "Use the head/tail breaks algorithm to identify the core contributors since
   the author contribution curve is usually a heavy tailed distribution."
  [entity-ownership devs-sorted-on-loc]
  (let [total-code-size (total-code-size-with-ex-devs entity-ownership) ; includes ex-devs
        sc  (authors-at-head-of-power-law 0.1 devs-sorted-on-loc)]
    (if (empty? sc)
      (let [second-attempt (authors-at-head-of-power-law 0.3 devs-sorted-on-loc)] ; might happen for "flat" contribution curves early in a project
        (if (empty? second-attempt)
          (pick-minimum-amount-of-personnel total-code-size devs-sorted-on-loc)
          (personnel-statistics-for second-attempt total-code-size)))
      (personnel-statistics-for sc total-code-size))))

(defn key-developer-statistics
  [ex-devs entity-ownership]
  (->> entity-ownership
       (remove (comp ex-devs :author))
       loc-per-dev
       (statistics-for-authors-at-head-of-power-law entity-ownership)))

(defn key-developer-statistics-from
  [entity-ownership-csv-file lost-authors-csv-file]
  (let [ex-devs (->> lost-authors-csv-file sc/slurp-csv (map :author) set)]
    (->> entity-ownership-csv-file
         sc/slurp-csv
         (sc/cast-with {:author-added sc/->int})
         (key-developer-statistics ex-devs))))
