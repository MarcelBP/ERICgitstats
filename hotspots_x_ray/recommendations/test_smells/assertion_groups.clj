(ns hotspots-x-ray.recommendations.test-smells.assertion-groups
  (:require [taoensso.timbre :as timbre]
            [hotspots-x-ray.languages.antlr :as antlr]
            [hotspots-x-ray.diagnostics.performance :as performance]
            [evolutionary-metrics.complexity.loco :as loco]
            [hotspots-x-ray.languages.specs :as parser-specs]
            [clojure.spec.alpha :as s]
            [hotspots-x-ray.recommendations.code-health.size-detectors :as size-detectors]
            [clojure.math.numeric-tower :as math])
  (:import (hotspots_x_ray.languages.testsmells CSharpRepeatedAssertions
                                                JavaRepeatedAssertions
                                                CppRepeatedAssertions
                                                EcmaScriptRepeatedAssertions
                                                ScalaRepeatedAssertions)
           (hotspots_x_ray.languages.generated CSharpConsecutiveAssertionsLexer CSharpConsecutiveAssertionsParser
                                               JavaConsecutiveAssertionsLexer JavaConsecutiveAssertionsParser
                                               CppConsecutiveAssertionsLexer CppConsecutiveAssertionsParser
                                               EcmaScriptConsecutiveAssertionsLexer EcmaScriptConsecutiveAssertionsParser
                                               ScalaConsecutiveAssertionsLexer ScalaConsecutiveAssertionsParser)
           (java.text ParseException)))

;; This module detects three types of test smells:
;;
;; 1. Multiple assertions (at least 4) repeated after each other -> "communicate the test criteria in the language of your domain".
;; 2. Duplicated assertion groups. At least 3 assertions where the criteria is duplicated.
;; 3. Multiple (two enough) assertion groups inside the same function.
;;
;; Note that the test smells are only invoked if we can deduce that it is a likely test file.

(defn- to-internal-assertion-format
  "Transforms a sequence representing repeated assertion statements to the internal
   format used in the analysis.
   The input data from the parser is given as List<List<String>> where
   each element is a list of the repeated assertions. The assertions themselves are
   strings."
  [ags]
  (let [assertions (.assertionGroups ags)
        block-sizes (map count assertions)
        blocks-in-fn (count assertions)]
    {:n-assertion-blocks blocks-in-fn
     :assertion-block-sizes block-sizes
     :raw-assertions assertions}))

(def ^:private cpp-parser
  (antlr/make-parser {:lexer-ctor    #(CppConsecutiveAssertionsLexer. %1)
                      :parser-ctor   #(CppConsecutiveAssertionsParser. %1)
                      :context-fn    #(.singlefunctionscope %1)
                      :listener-ctor #(CppRepeatedAssertions.)}))

(def ^:private extended-ecma-script-parser
  (antlr/make-parser {:lexer-ctor    #(EcmaScriptConsecutiveAssertionsLexer. %1)
                      :parser-ctor   #(EcmaScriptConsecutiveAssertionsParser. %1)
                      :context-fn    #(.singlefunctionscope %1)
                      :listener-ctor #(EcmaScriptRepeatedAssertions.)}))

(def ^:private scala-parser
  (antlr/make-parser {:lexer-ctor    #(ScalaConsecutiveAssertionsLexer. %1)
                      :parser-ctor   #(ScalaConsecutiveAssertionsParser. %1)
                      :context-fn    #(.singlefunctionscope %1)
                      :listener-ctor #(ScalaRepeatedAssertions.)}))

(def ^:private assertion-parsers
  {".c"    cpp-parser
   ".h"    cpp-parser
   ".cc"   cpp-parser
   ".cpp"  cpp-parser
   ".cxx"  cpp-parser
   ".hh"   cpp-parser
   ".hpp"  cpp-parser
   ".hxx"  cpp-parser

   ".js"    extended-ecma-script-parser
   ".ts"    extended-ecma-script-parser
   ".jsx"   extended-ecma-script-parser
   ".tsx"   extended-ecma-script-parser
   ".vue"   extended-ecma-script-parser


   ".cs" (antlr/make-parser {:lexer-ctor    #(CSharpConsecutiveAssertionsLexer. %1)
                             :parser-ctor   #(CSharpConsecutiveAssertionsParser. %1)
                             :context-fn    #(.singlefunctionscope %1)
                             :listener-ctor #(CSharpRepeatedAssertions.)})

   ".java" (antlr/make-parser {:lexer-ctor    #(JavaConsecutiveAssertionsLexer. %1)
                               :parser-ctor   #(JavaConsecutiveAssertionsParser. %1)
                               :context-fn    #(.singlefunctionscope %1)
                               :listener-ctor #(JavaRepeatedAssertions.)})
   ".scala" scala-parser})

(defn- function-level
  [file-path {:keys [body]}]
  (let [ext (loco/extension-of file-path)
        parser (get assertion-parsers ext)]
      (try
        (performance/with-timbre-exe-time-info
          (parser to-internal-assertion-format file-path body))
        (catch Throwable e
          (do
            (timbre/error "X-Ray assertion groups failed to parse the file " file-path e)
            (throw (ParseException. (str "X-Ray assertion groups failed to parse " file-path) 0)))))))

(defn- parse-to-named-test-function
  [file-path {:keys [name] :as fn-info}]
  (merge {:name name} (function-level file-path fn-info)))

(defn- any-assertions?
  [{:keys [n-assertion-blocks]}]
  (< 0 n-assertion-blocks))

(defn- parse-assert-info
  [file-path raw-fns]
  (->> raw-fns
       (map (partial parse-to-named-test-function file-path))
       (filter any-assertions?)))

(defn- minimum-number-of-asserts-for-clone-detection?
  "Ignore one or two line asserts since that duplication might be OK."
  [raw-assertions]
  (< 2 (count raw-assertions)))

(defn- to-named-assert-blocks
  [assertions]
  (reduce (fn [acc {:keys [name raw-assertions]}]
            (let [candidates (filter minimum-number-of-asserts-for-clone-detection? raw-assertions)
                  named (map (fn [c] {:name name :body c}) candidates)]
              (concat acc named)))
          []
          assertions))

(defn- duplicated-block?
  [a1 a2]
  (= a1 a2)) ; Note: strong equality; we could use similarity too, which would be better...

(defn- duplicated-blocks?
  [a1 a2]
  (let [duplicated-lines (for [c a1
                               :let [duplicates (filter (partial duplicated-block? c) a2)
                                     duplicate? (seq duplicates)]]
                           (some? duplicate?))]
    (some true? duplicated-lines)))

(defn- fns-with-duplicated-assertions
  [assertions]
  (let [candidates (to-named-assert-blocks assertions)
        duplicated-fns (for [c candidates
                             :let [n (:name c)
                                   b (:body c)
                                   duplicates (->> candidates
                                                   (remove (comp (partial = n) :name))
                                                   (filter (comp (partial duplicated-blocks? b) :body))
                                                   (map :name)
                                                   distinct)]
                             :when (seq duplicates)]
                         {:name n :clones duplicates})]
    duplicated-fns))

(def ^:private no-assertions-found {})

(def ^:private lines-in-large-assert-block 4)

; If one assertion block is copied to at least two other tests:
(def ^:private threshold-for-many-assertion-clones 2)

(defn- large-blocks-in
  [assertions]
  (->> assertions
       (map :assertion-block-sizes)
       flatten
       (filter (partial <= lines-in-large-assert-block))))

(defn- max-assertion-blocks
  [assertions]
  (if (empty? assertions)
    no-assertions-found
    (let [max-assertion-blocks (apply max-key :n-assertion-blocks assertions)
          max-assertion-block-sizes (apply max-key (comp count :assertion-block-sizes) assertions)
          large-blocks (large-blocks-in assertions)]
      {:max-assertion-blocks {:name (:name max-assertion-blocks)
                              :n-blocks (:n-assertion-blocks max-assertion-blocks)}
       :max-assertion-block-sizes {:name (:name max-assertion-block-sizes)
                                   :n-lines (apply max (:assertion-block-sizes max-assertion-block-sizes))}
       :n-large-assertion-blocks (count large-blocks)
       :n-fns-with-assertions (count assertions)})))

(defn- clone-statistics
  [fns-with-duplicates]
  (if (empty? fns-with-duplicates)
    no-assertions-found
    (let [most-clones (apply max-key (comp count :clones) fns-with-duplicates)
          fns-with-many-clones (->> fns-with-duplicates
                                    (map :clones)
                                    (map count)
                                    (filter (partial <= threshold-for-many-assertion-clones)))]
      {:test-with-many-duplicated-asserts {:name (:name most-clones)
                                            :clones (:clones most-clones)}
       :n-tests-with-many-clones (count fns-with-many-clones)})))

(defn- assertion-stats-from
  [assertions fns-with-duplicates]
  (-> {}
      (merge (max-assertion-blocks assertions))
      (merge (clone-statistics fns-with-duplicates))))

(defn test-code?
  [file-path]
  (->> file-path
       clojure.string/lower-case
       (re-find #"test")
       some?))

(s/def ::n-lines nat-int?)
(s/def ::name string?)

(s/def ::max-assertion-block-sizes (s/keys :req-un [::n-lines
                                                    ::name]))
(s/def ::n-blocks nat-int?)
(s/def ::max-assertion-blocks (s/keys :req-un [::n-blocks
                                               ::name]))
(s/def ::n-fns-with-assertions nat-int?)
(s/def ::n-large-assertion-blocks nat-int?)
(s/def ::n-tests-with-many-clones nat-int?)

(s/def ::clones (s/coll-of string?))
(s/def ::test-with-many-duplicated-asserts (s/keys :req-un [::clones
                                                             ::name]))

(s/def ::assertion-metrics (s/keys :opt-un [::max-assertion-block-sizes
                                            ::max-assertion-blocks
                                            ::n-fns-with-assertions
                                            ::n-large-assertion-blocks
                                            ::n-tests-with-many-clones
                                            ::test-with-many-duplicated-asserts]))

(defn metrics
  [file-path raw-fns]
  (if (and (antlr/supports-file-type? file-path assertion-parsers)
           (test-code? file-path))
    (let [assertions (parse-assert-info file-path raw-fns)
          fns-with-duplicates (fns-with-duplicated-assertions assertions)]
      {:assertions (assertion-stats-from assertions fns-with-duplicates)})
    no-assertions-found))

(s/fdef metrics
        :args (s/cat :file-path string?
                     :raw-fns ::parser-specs/parsed-function-statistics)
        :ret ::assertion-metrics)

;; The specific biomarkers built on the metrics calulcated above.
;;

(defn large-assertion-blocks?
  "Warns if the unit contains more than a specified number of large assertion blocks.
   That is, multiple test cases have long multi-line assert criteria."
  [ths {:keys [assertions] :as stats}]
  (when (some? assertions)
    (let [{:keys [n-large-assertion-blocks] :or {n-large-assertion-blocks 0}} assertions]
      (and (not (size-detectors/few-functions? ths stats))
           (>= n-large-assertion-blocks (:many-large-assertion-blocks ths))))))

(defn penalties-for-large-assertion-blocks
  [{:keys [assertions] :as _stats}]
  (-> assertions
      :n-large-assertion-blocks
      (/ 2)
      math/round
      (min 7)))

(defn describe-large-assertion-blocks
  [{:keys [assertions]}]
    (str "The test suite contains " (:n-large-assertion-blocks assertions) " assertion blocks with at least "
         lines-in-large-assert-block " consecutive assertions each. "
         "Extract and put a name on those blocks to communicate the test criteria in the language of your domain."))

(defn duplicated-assertion-blocks?
  [_ths {:keys [assertions] :as _stats}]
  (when (some? assertions)
    (let [{:keys [n-tests-with-many-clones test-with-many-duplicated-asserts] :or
                 {n-tests-with-many-clones 0 test-with-many-duplicated-asserts {}}} assertions
          {:keys [clones]} test-with-many-duplicated-asserts]
      (and (some? clones)
           (<= threshold-for-many-assertion-clones (count clones))
           (< threshold-for-many-assertion-clones n-tests-with-many-clones)))))

(defn penalties-for-duplicated-assertion-blocks
  [{:keys [assertions]}]
  (-> assertions
      :test-with-many-duplicated-asserts
      :clones
      count
      (/ 2)
      math/round
      (min 7)))

(defn describe-duplkicated-assertion-blocks
  [{:keys [assertions]}]
  (let [{:keys [clones name]} (:test-with-many-duplicated-asserts assertions)
        n-clones (count clones)
        presentable-clones (->> clones (take 5) (clojure.string/join ", "))
        n-clones-left (- n-clones (count presentable-clones))]
    (str "The test suite contains similar assert blocks. The test " name " has similar assert statements in " presentable-clones
         (if (>= n-clones-left 2)
           (str " and " n-clones-left " more tests.")
           "."))))
