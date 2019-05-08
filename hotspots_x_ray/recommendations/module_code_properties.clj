(ns hotspots-x-ray.recommendations.module-code-properties
  (:require [hotspots-x-ray.languages.parser :as language-parser]
            [hotspots-x-ray.recommendations.code-complexity :as complexity]
            [hotspots-x-ray.recommendations.similarity :as similarity]
            [hotspots-x-ray.recommendations.function-arguments :as fn-args]
            [hotspots-x-ray.recommendations.code-extraction :as extraction]
            [evolutionary-metrics.complexity.loco :as loco]
            [evolutionary-metrics.complexity.loco-rules :as loco-rules]
            [hotspots-x-ray.recommendations.test-smells.assertion-groups :as assertion-smells]
            [hotspots-x-ray.recommendations.code-comments.pattern-statistics :as code-patterns]
            [codescene.stats.math :as m]
            [clojure.math.combinatorics :as combo]))

;; This module calculates descriptive statistics over a file, such as the number of functions/methods, the
;; total length of the active code, etc.
;; Those statistics are then used as part of our quality scores.

(defn- code-field
  [[_blanks _comments code]]
  code)

(defn- code-by-loco
  [language-rules lines]
  (-> lines (loco/code-stats-in language-rules) code-field))

(defn- lines-of-code-in
  "Use loco to strip away whitespace and comments."
  [language-rules {:keys [body name]}]
  {:name name
   :lines (->> body clojure.string/split-lines (code-by-loco language-rules))})

(defn- sum-active-code-for
  [fn-locs]
  (reduce + 0 fn-locs))

(defn- longest-function-loc
  "Typically used to identify potential God methods."
  [named-fn-locs]
  (if (empty? named-fn-locs)
    {:name "<unknown>" :lines 0}
    (apply max-key :lines named-fn-locs)))

(defn- raw-functions-in
  [lines-in-file functions]
  (extraction/code-by-function-name lines-in-file functions))

(def ^:private cut-off-point-for-high-duplication 75) ; percent

(defn- clone-statistics
  [fns clone-similarities]
  (let [n-clones (count clone-similarities)]
    {:n-clones n-clones
     :clone-ratio (/ n-clones (max 1 (count fns)))}))

;; NOTE: the copy-paste detection is incredibly slow. As a "solution" we skip this
;; code property on large files since large files are 1) perhaps code generated or
;; 2) might have other more serious problems.

(def ^:private no-code-clones {:n-clones 0 :clone-ratio 0.0})

(def ^:private largest-file-size-for-copy-paste-detection 5000)

(defn- copy-paste-thresholds-depending-on
  [active-code-size]
  (cond
    (< active-code-size 2000)
    {:min-fn-length 300
     :max-body-length-delta-in-percent 60}

    (< active-code-size largest-file-size-for-copy-paste-detection)
    {:min-fn-length 600
     :max-body-length-delta-in-percent 70}

    :else
    {:min-fn-length 1000
     :max-body-length-delta-in-percent 85}))

(defn- small-enough-to-run-copy-paste?
  [active-code-size]
  (<= active-code-size largest-file-size-for-copy-paste-detection))

(defn- detect-named-similarity
  [thresholds [f1 f2]]
  (merge {:n1 (:name f1)
          :n2 (:name f2)}
         (similarity/similarity-in-percent f1 f2 thresholds)))

(defn- unique-clones-in
  "Since we use combo/combinations, we end up with the same function mapped to multiple clones.
   Filter so that we get a list of all unique names."
  [similar-fns]
  (->> similar-fns
       (map (juxt :n1 :n2))
       (apply concat)
       distinct))

(defn- copy-paste-index
  [active-code-size fns]
  (if (small-enough-to-run-copy-paste? active-code-size)
    (->> (combo/combinations fns 2)
         (map (partial detect-named-similarity (copy-paste-thresholds-depending-on active-code-size)))
         (remove :ignored)
         (filter (comp (partial < cut-off-point-for-high-duplication) :similarity))
         unique-clones-in
         (clone-statistics fns))
    no-code-clones))

(defn- safe-max-cc
  [named-ccs]
  (if (empty? named-ccs)
    {:name "<unknown>" :cc 0}
    (apply max-key :cc named-ccs)))

(defn- complexity-stats
  [named-ccs]
  (let [ccs (map :cc named-ccs)
        {:keys [median mean]} (m/stats-for ccs)
        max-cc (safe-max-cc named-ccs)
        total (reduce + ccs)]
    {:cc-median median
     :cc-mean mean
     :cc-max (:cc max-cc)
     :cc-max-name (:name max-cc)
     :cc-total total}))

(defn- complexity-metrics
  [file-path lines-in-file functions]
  (->> functions
       (complexity/by-functions-in-file file-path lines-in-file)
       (map (fn [{:keys [cc] :as v}] (assoc v :cc (Integer/parseInt cc))))
       complexity-stats))

(defn- main-body-complexity
  [file-path input cc-stats]
  (let [total (complexity/total-in-file file-path input)]
    (max (- total (:cc-total cc-stats)) 0)))

(defn- nested-complexity-stats
  [file-path {:keys [nested-complexity-of-interest] :as _options} input named-nested]
  (let [ordered (sort-by :nested-complexity > named-nested)
        max-nested (or (first ordered) {:name "<unknown>" :nested-complexity 0})
        interesting-nested (take-while (comp (partial <= nested-complexity-of-interest) :nested-complexity) ordered)
        max-nested-in-global-scope (complexity/max-nested-complexity-in-file file-path input)]
    {:max-nested-complexity-depth (:nested-complexity max-nested)
     :max-nested-complexity-depth-name (:name max-nested)
     :nested-complexity-of-interest (map :nested-complexity interesting-nested)
     :max-nested-in-global-scope max-nested-in-global-scope}))

(defn- nested-complexity
  [file-path options input functions]
  (->> functions
       (complexity/by-deepest-nested-in-file file-path input)
       (remove (comp (partial = complexity/unknown-complexity-result) :nested-complexity))
       (map (fn [{:keys [nested-complexity] :as v}] (assoc v :nested-complexity (Integer/parseInt nested-complexity))))
       (filter (comp (partial < 2) :nested-complexity))
       (nested-complexity-stats file-path options input)))

(defn of
  [project file-path input options]
  (let [lines-in-file (clojure.string/split-lines input)
        fns (language-parser/parse-function-statistics file-path input)
        raw-fns (raw-functions-in lines-in-file fns)
        language-rules (-> file-path loco/extension-of loco-rules/rule-for)
        named-fn-locs (map (partial lines-of-code-in language-rules) raw-fns)
        fn-locs (map :lines named-fn-locs)
        {:keys [median]} (m/stats-for fn-locs)
        max-fn-loc (longest-function-loc named-fn-locs)
        active-code-size (sum-active-code-for fn-locs)
        clones (copy-paste-index active-code-size fns)
        cc (complexity-metrics file-path lines-in-file fns)
        main-body-cc (main-body-complexity file-path input cc)
        fn-arg-metrics (fn-args/function-argument-metrics file-path input raw-fns)
        nested (nested-complexity file-path options input fns)
        assertion-metrics (assertion-smells/metrics file-path fns)
        comments (code-patterns/comments-matching project file-path input)]
    (merge clones
           fn-arg-metrics
           assertion-metrics
           comments
           cc
           {:n-functions (count fns)
            :lines-in-file (code-by-loco language-rules lines-in-file)
            :nested nested
            :main-body-cc main-body-cc
            :active-code-size active-code-size
            :longest-fn-loc (:lines max-fn-loc)
            :longest-fn-loc-name (:name max-fn-loc)
            :median-fn-loc median}))) ; When combined with longest-fn-loc, we can see if there are a few God methods or long methods in general

