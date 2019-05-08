;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.mining.ws-complexity
  (:require [evolutionary-metrics.complexity.loco :as loco]
            [clojure.string :as string]
            [incanter.stats :as stats]))

;;; This module calculates complexity metrics using the indentation of 
;;; the source code as a proxy for complexity. 
;;;
;;; The idea of using indentation as a proxy for complexity is backed by research. 
;;; It's a simple metric, yet it correlates medium to high with more elaborate metrics
;;; such as McCabe Cyclomatic Complexity and Halstead complexity measures.
;;; The advantages are:
;;;  - Language independent: no need for a separate parser for each programming language 
;;;    in polyglot code bases.
;;;  - Calculates incomplete code: perfect for code snippets or partial code.
;;;  - Speed: indentation is a simple and fast metric that quickly hints at the underlaying complexity.

(defn- trim-indent
  "For simplicity we drop all matches - doesn't
   really matter what comes after the inital
   white space anyway."
  [line regexp]
  (string/replace line regexp ""))

(defn trim-leading-spaces
  [line]
  (trim-indent line #"[ ]"))

(defn trim-leading-tabs
  [line]
  (trim-indent line #"[\t]"))

(defn leading-tabs-count
  "Returns the number of leading tabs, each forms
   a logical indent."
  [line]
  (->>
   (trim-leading-spaces line)
   (re-find #"^\t+")
   count))

(defn leading-spaces-count
  "Returns the number of leading spaces.
   Note that this is a raw number and has
   to be adjusted to a logical indent."
  [line]
  (->>
   (trim-leading-tabs line)
   (re-find #"^ +")
   count))

(defn as-logical-indents
  "Returns the total number of logical indents at
   the start of the given line."
  [{:keys [tabs spaces]} line]
  (let [raw-tabs (leading-tabs-count line)
        logical-tabs (/ raw-tabs tabs)
        raw-spaces (leading-spaces-count line)
        logical-spaces (/ raw-spaces spaces)]
    (+ logical-tabs logical-spaces 1))) ; we consider no indent as complexity "1" - gets silly compared to comments otherwise

;; Specify functions for calculating the statistics.
;; Each of these functions operate on a sequence of
;; lines as specified by their indentation:

(defn- as-presentation-value
  [v]
  (if (Double/isNaN v)
    0.00
    (as-> v res
          (Math/abs res)
          (* 100.0 res)
          (Math/round res)
          (/ res 100.0))))

(def total (comp float (partial reduce +)))

(def mean (comp as-presentation-value stats/mean))

(def median (comp as-presentation-value stats/median))

(def sd (comp as-presentation-value stats/sd))

(defn max-c [v]
  (if (seq v)
    (int (apply max v))
    0))

(defn complexity-by-loc
  [complexity n-lines]
  (as-presentation-value (/ complexity (max n-lines 1.0))))

(deftype ComplexityStatsAccumulator [stats-counter code-lines options]
  loco/CodeStatsAccumulator
  (blanks [this] (ComplexityStatsAccumulator. (loco/blanks stats-counter) code-lines options))
  (comments [this line] (ComplexityStatsAccumulator. (loco/comments stats-counter line) code-lines options))
  (codes [this line] (ComplexityStatsAccumulator. stats-counter (conj code-lines line) options))
  (in-multi? [this] (loco/in-multi? stats-counter))
  (start-multi-comment [this line] (ComplexityStatsAccumulator. (loco/start-multi-comment stats-counter line) code-lines options))
  (end-multi-comment [this line] (ComplexityStatsAccumulator. (loco/end-multi-comment stats-counter line) code-lines options))
  (stats [this]
    (let [indented-lines (map (partial as-logical-indents options) code-lines)
          [blank-lines comment-lines _] (loco/stats stats-counter)
          total-complexity (total indented-lines)
          n-lines (count indented-lines)]
      {:total total-complexity
       :n n-lines
       :mean (mean indented-lines)
       :median (median indented-lines)
       :sd (sd indented-lines)
       :max (max-c indented-lines)
       :blanks blank-lines
       :comments comment-lines
       :complexity-loc-ratio (complexity-by-loc total-complexity n-lines)})))

(defn total-indent-complexity
  "Accumulates the total complexity (expressed as
   leading logical units of indentation) of the
   given seq of lines."
  [options lines language-rules]
  (->> (ComplexityStatsAccumulator. (loco/make-line-count-accumulator) [] options)
       (loco/reduce-stats-in lines language-rules)
       .stats))
