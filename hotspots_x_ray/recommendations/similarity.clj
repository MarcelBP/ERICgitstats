(ns hotspots-x-ray.recommendations.similarity
  (:require [taoensso.timbre :as timbre]
            [evolutionary-metrics.analysis.math :as math]
            [hotspots-x-ray.diagnostics.performance :as diagnostics])
  (:import hotspots_x_ray.algorithms.LevenshteinDistance))

(defn- levenshtein
  [b1 b2]
  (diagnostics/with-timbre-exe-time-info
    (LevenshteinDistance/levenshteinDistance b1 b2)))

(defn- passes-thresholds?
  [{:keys [min-fn-length max-body-length-delta-in-percent] :or {min-fn-length 200 max-body-length-delta-in-percent 60}}
   largest
   smallest]
  (and (> largest min-fn-length)
       (< max-body-length-delta-in-percent (math/as-percentage (/ smallest largest)))))

(defn similarity-in-percent
  "Calculates the similarity as the amount of duplicated code
   between the given functions.
   The algorithm is based on the Levenshtein distance between
   the body of the two functions.
   We ignore similarities between too small functions because they're likely to
   result in larger errors. Since we don't know the number of lines we put a
   minimum threshold on the number of characters needed for a sensible calulcation."
  [f1 f2 thresholds]
  {:pre  [(some? (:body f1)) (some? (:body f2))]}
  (let [b1 (:body f1)
        b2 (:body f2)
        b1-length (count b1)
        b2-length (count b2)
        largest-body (max b1-length b2-length 1)
        smallest-body (min b1-length b2-length)]
      (if (passes-thresholds? thresholds largest-body smallest-body)
        (do
          (timbre/trace "Similarity calculation started between '" (:name f1) "' and '" (:name f2) "' with body sizes "
                        largest-body " and " smallest-body)
          (let [distance (levenshtein b1 b2)
                difference (- largest-body distance)
                similarity (int (math/as-percentage (/ difference largest-body)))]
            (timbre/trace "Similarity between '" (:name f1) "' and '" (:name f2) "' is "
                          similarity "% based on distance " distance " and a body of size " largest-body)
            {:similarity similarity}))
        (do
          (timbre/trace "Similarity calculation ignored between '" (:name f1) "' and '" (:name f2) "' because "
                        "the maximum body sizes " largest-body " and " smallest-body " are below the thresholds")
          {:ignored true :similarity 0}))))
