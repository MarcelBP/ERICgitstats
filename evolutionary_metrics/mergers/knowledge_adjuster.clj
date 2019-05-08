(ns evolutionary-metrics.mergers.knowledge-adjuster
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [evolutionary-metrics.mergers.shared-mergers :as shared]))

;;; Problem
;;; =======
;;; Our social knowledge statistics measures the amount of historic contribution in relationship to the
;;; total contribution. This metric requires the analysis to be run on the whole life time of the project, otherwise
;;; we get a bias.
;;; For example, let's say we have a file with the 1.000 LoC. If I'm the only one who contributes a change to that
;;; file in the analysis period, I'll get 100% ownership _independent_ of how much code I contribute. This is clearly a
;;; problem.
;;;
;;; Our Solution
;;; ============
;;; The algorithms in this module tries to minimize the bias by adjusting for the total code size of the current file.
;;;
;;; Note that the adjustment isn't perfect - the historic contribution may well be larger than the current file size.

(def ^:const unkown-main-dev-name "unknown")

(defn- data-for-whole-history?
  "Well, actually we cannot tell for sure here - this is more like
   'enough data to consider the original metric OK'"
  [code-size total-added]
  (< code-size total-added))

(defn- ownership-of
  [added total]
  (-> added (/ total) float str))

(defn- adjusted-developer-ownership
  "Note only invoked if we detect that we lack significant data.
   In that case, we adjust with respect to the current code size (it's a
   heuristic after all)."
  [code-size main-dev added total-added]
  (let [unknown-loc (- code-size total-added)
        unknown-ownership (ownership-of unknown-loc code-size)
        main-dev-ownership (ownership-of added code-size)]
    (if (> unknown-loc added)
      [unknown-loc unknown-ownership unkown-main-dev-name]
      [added main-dev-ownership main-dev])))

(defn adjust-ownership-for-entity
  [entity main-dev added total-added ownership code-size]
  (if (data-for-whole-history? code-size total-added)
    [entity main-dev added total-added ownership]
    (let [[adjusted-added adjusted-ownership adjusted-main-dev] (adjusted-developer-ownership code-size main-dev added total-added)]
      [entity adjusted-main-dev adjusted-added code-size adjusted-ownership])))

(defn adjust-by
  [complexity main-devs]
  (for [[entity main-dev added total-added ownership] main-devs
        :let [code-size (get complexity entity)]
        :when code-size]
    (adjust-ownership-for-entity entity main-dev (Integer/parseInt added) (Integer/parseInt total-added) ownership (max 1 (Integer/parseInt code-size)))))

(defn- size-lookup-by-name
  [complexity-file]
  (into {} (shared/modules-with-complexity complexity-file)))

(defn adjust-ownership-by-code-size
  ([complexity-file main-dev-file]
   (let [main-devs (shared/read-csv-sans-header-from main-dev-file)
         complexity (size-lookup-by-name complexity-file)
         adjusted (adjust-by complexity main-devs)]
     (csv/write-csv *out* [["entity" "main-dev" "added" "total-added" "ownership"]])
     (csv/write-csv *out* adjusted)))
  ([complexity-file main-dev-file out-file-name]
   (with-open [out-file (io/writer out-file-name)]
     (binding [*out* out-file]
       (adjust-ownership-by-code-size complexity-file main-dev-file)))))
