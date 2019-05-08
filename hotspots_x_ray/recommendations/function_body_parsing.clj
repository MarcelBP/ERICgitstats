(ns hotspots-x-ray.recommendations.function-body-parsing
  (:require [hotspots-x-ray.recommendations.code-extraction :as extraction]))

;; This module encapsulates the special cases involved in the base data for biomarkers.
;;

(def ^:private general-special-cases #{".swift" ".vb" ".go" ".rb" ".kt" ".pl" ".pm" ".scala" ".erl"})

(defn function-adapter-for
  "A special case as the languages specified in this module don't have any
   delimiters between its control structures (e.g. no parentheses).
   Hence, we cannot parse it properly once the whitespace has been stripped away."
  ([ext input]
   (function-adapter-for ext input #{}))
  ([ext input additional-special-case]
   (let [special-case? (clojure.set/union general-special-cases additional-special-case)]
     (if (special-case? ext)
       (partial (partial extraction/code-by-function-name (clojure.string/split-lines input)))
       identity))))
