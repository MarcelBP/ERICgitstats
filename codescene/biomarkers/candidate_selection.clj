(ns codescene.biomarkers.candidate-selection
  (:require [incanter.stats :as stats]
            [taoensso.timbre :as log]))

; we want at least this amount of biomarkers
(def ^:private least-amount-of-desired-candidates 10)

(defn- select-candidates-from
  [hs]
  (let [hs-length (max (count hs) 1)]
    (loop [head hs]
      (if (empty? head) ; edge case -> handle in calling context
        head
        (let [mean (stats/mean (map :revisions head))
              safe-mean (if (Double/isNaN mean) 0 mean)
              new-head (filter #(> (:revisions %) safe-mean) head)
              new-head-count (count new-head)
              cut-off-point (/ new-head-count hs-length)]
          (log/debug "Detecting biomarker candidates: m = " safe-mean ", nhc = " new-head-count ", cof = " (float cut-off-point))
          (if (or (< new-head-count least-amount-of-desired-candidates)
                  (< cut-off-point 0.1)) ; original recommended value is 0.4
            new-head ; we've found our candidates
            (do ; continue to search...
              (log/debug "Detecting biomarker candidates: " new-head-count " candidates left out of a total of " hs-length)
              (recur new-head))))))))

;; If the last selected hotspot is very close to the next in terms of revisions, then
;; we want to include those as well.
(def ^:private closed-encounter-delta-revisions 2)

(defn- find-close-encounters-to
  [{:keys [revisions] :as last-selected-hostpos} hs-tail]
  (if (> revisions closed-encounter-delta-revisions)
    (let [cut-off (- revisions closed-encounter-delta-revisions)]
      (->> hs-tail
           (take-while (fn [h] (>= (:revisions h) cut-off)))
           (take 15))) ; some sanity value in case of flat distributions
    [])) ; probably a very small codebase...

(defn- needs-more-candidates?
  [cs]
  (< (count cs) least-amount-of-desired-candidates))

(def max-hotspots-to-take 200)

(defn head-of-power-law
  "Use the head/tail breaks algorithm to select the candidates for biomarker scores in our
   heavy tailed distribution of change frequencies in the hotspots."
  [all-hotspots]
  (let [sc  (->> all-hotspots
                 (take max-hotspots-to-take) ; no need to include more
                 select-candidates-from)
        n-sc (count sc)
        hs-tail (drop n-sc all-hotspots)]
    (if (empty? sc)
      (take least-amount-of-desired-candidates all-hotspots)
      (if (needs-more-candidates? sc)
        (concat sc (find-close-encounters-to (last sc) hs-tail))
        sc))))
