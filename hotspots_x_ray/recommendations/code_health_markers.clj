(ns hotspots-x-ray.recommendations.code-health-markers
  (:require [hotspots-x-ray.recommendations.cohesion :as cohesion]
            [hotspots-x-ray.recommendations.module-code-properties :as code-properties]
            [hotspots-x-ray.recommendations.language-specific-biomarker-rules :as language-rules]
            [clojure.spec.alpha :as s]
            [hotspots-x-ray.recommendations.code-biomarkers-spec :as biomarkers-spec]
            [hotspots-x-ray.recommendations.code-health.size-detectors :as size-detectors]
            [hotspots-x-ray.recommendations.test-smells.assertion-groups :as assertion-smells]
            [taoensso.timbre :as log]
            [evolutionary-metrics.complexity.loco :as loco]
            [clojure.math.numeric-tower :as math]))

(def ^:const no-score 0)

(def ^:const pefect-health 10)
(def ^:const health-warning 7)
(def ^:const bad-health 4)
(def ^:const game-over 1)

(defn- penalize-when
  ([desc criteria acc]
   (penalize-when desc criteria (constantly 1) acc))
  ([desc criteria penalty-points-fn acc]
   (if (criteria)
     (let [penalties (penalty-points-fn)]
       (log/debug "Penalizing file because of: " desc " with " penalties " points")
       (+ acc penalties))
     acc)))

(defn- god-function-cc-exists?
  [{:keys [god-function-cc]}
   {:keys [cc-max]}]
  (> cc-max god-function-cc))

(defn- god-function-loc-exists?
  [{:keys [god-function-loc]}
   {:keys [longest-fn-loc]}]
  (> longest-fn-loc god-function-loc))

(defn- god-function-exists?
  [ths stats]
  (or (god-function-cc-exists? ths stats)
      (god-function-loc-exists? ths stats)))

(defn- god-function-loc-and-cc-hit-same-target?
  "A God function is identified by both LoC and CC; When they're in
   the same function, we punish it harder."
  [{:keys [cc-max-name longest-fn-loc-name]}]
  (= cc-max-name longest-fn-loc-name))

(defn- penalties-for-god-function-cc
  [{:keys [cc-max]}]
  (-> cc-max
      (/ 10) ; "high complexity"
      math/round
      inc
      (max 3) ; always punish, even though it's "just" the LoC rule that strikes
      (min 10)))

(defn- penalties-for-god-function-loc
  [{:keys [god-function-loc]} {:keys [longest-fn-loc]}]
  (-> longest-fn-loc
      (/ god-function-loc) ; how many times to we exceed the threshold?
      math/round
      (max 1) ; minimum penalty points
      (min 4))) ; maximum penalty points

(defn- penalties-for-god-function
  [ths stats]
  (if (god-function-loc-and-cc-hit-same-target? stats)
    (* (penalties-for-god-function-cc stats)
       (if (god-function-loc-exists? ths stats)
         (max (penalties-for-god-function-loc ths stats) 2)
         1))
    (+ (if (god-function-cc-exists? ths stats)
         (penalties-for-god-function-cc stats)
         0)
       (if (god-function-loc-exists? ths stats)
         (penalties-for-god-function-loc ths stats)
         0))))

(defn- dear-god-function-exists?
  [{:keys [dear-god-function-cc dear-god-function-loc]}
   {:keys [cc-max longest-fn-loc]}]
  (or (> cc-max dear-god-function-cc)
      (> longest-fn-loc dear-god-function-loc)))

(defn- some-code-clones?
  [ths {:keys [n-clones]}]
  (> n-clones (:n-clones ths)))

(defn- high-degree-of-duplication?
  [ths {:keys [clone-ratio] :as stats}]
  (when-not (some-code-clones? ths stats) ; don't double punish
    (when-not (size-detectors/few-functions? ths stats)
      (> clone-ratio (:clone-ratio ths)))))

(defn- penalties-for-code-clones
  [file-name {:keys [n-clones]}]
  (if (assertion-smells/test-code? file-name) ; allow more duplication in tests
    (-> n-clones
        (/ 3)
        math/round
        (min 5))
    (-> n-clones ; application code is punished harder
        (/ 2)
        math/round
        (min 7))))

(defn- nested-logic?
  [{:keys [nested-complexity-of-interest] :as _ths} {:keys [nested]}]
  (when nested
    (let [{:keys [max-nested-complexity-depth]} nested]
      (<= nested-complexity-of-interest max-nested-complexity-depth))))

(defn- nested-logic-penalties-for
  [{:keys [nested-complexity-of-interest] :as _ths} {:keys [nested] :as _stats}]
  (->> nested
      :nested-complexity-of-interest
       (map (fn [n] (max 1 (- n nested-complexity-of-interest))))
       (reduce +)
       (max 2)))

(defn- deep-global-nested-logic?
  [{:keys [nested-complexity-of-interest] :as ths} {:keys [nested] :as stats}]
  (when-not (nested-logic? ths stats)
    (when (some? (:max-nested-in-global-scope nested))
      (<= nested-complexity-of-interest (:max-nested-in-global-scope nested)))))

(defn- deep-global-nesteing-penalties-for
  [{:keys [nested] :as _stats}]
  (->> nested
       :max-nested-in-global-scope
       (max 2)))

(defn- many-conditionals?
  [ths {:keys [cc-mean] :as props}]
  (and (not (size-detectors/few-functions? ths props))
       (> cc-mean (:cc-mean ths))))

(defn- many-conditionals-penalties-for
  [ths {:keys [cc-mean] :as _props}]
  (-> cc-mean
      (- (:cc-mean ths))
      (/ (:cc-mean ths))
      Math/round
      (max 2)))

(defn- more-than-one-function?
  [_ths {:keys [n-functions]}]
  (< 1 n-functions))

(defn- many-global-conditionals?
  [ths {:keys [main-body-cc] :as props}]
  (when-not (many-conditionals? ths props)
    (when (some? main-body-cc)
      (> main-body-cc (:main-body-cc ths)))))

(defn- many-global-conditionals-penalties-for
  [{:keys [main-body-cc] :as _props}]
  (-> main-body-cc
      (/ 10)
      math/round
      dec))

(defn- large-functions-in-general?
  [ths {:keys [median-fn-loc] :as stats}]
  (and (not (size-detectors/few-functions? ths stats))
       (> median-fn-loc (:median-fn-loc ths))))

(defn- missing-fn-arg-abstraction?
  [ths {:keys [fn-args] :as stats}]
  (when fn-args ; optional
    (let [{:keys [mean-args] :or {mean-args 0.0}} fn-args]
      (and (not (size-detectors/few-functions? ths stats))
           (> mean-args (:mean-fn-args ths))))))

(defn- many-primitives?
  [ths n-primitives]
  (< (:many-primitives ths) n-primitives))

(defn- primitive-args-ratio-from
  [{:keys [n-args n-primitives] :or {n-args 1 n-primitives 0}}]
  (/ n-primitives (max 1.0 n-args)))

(defn- primitive-obsession?
  [ths {:keys [fn-args] :as stats}]
  (when fn-args ; optional
    (let [{:keys [n-primitives] :or {n-primitives 0}} fn-args
          ratio (primitive-args-ratio-from fn-args)]
      (and (not (size-detectors/few-functions? ths stats))
           (many-primitives? ths n-primitives)
           (> ratio (:primive-fn-args-ratio ths))))))

(defn- any-excess-arguments?
  [ths {:keys [fn-args] :as _stats}]
  (when fn-args ; optional
    (let [{:keys [max-args] :or {max-args 0}} fn-args]
      (> max-args (:max-fn-args ths)))))

(defn- constructor-over-injection?
  [ths {:keys [fn-args] :as _stats}]
  (when fn-args ; optional
    (let [{:keys [max-ctor-args] :or {max-ctor-args 0}} fn-args]
      (> max-ctor-args (:max-ctor-args ths)))))

(defn- string-heavy-arguments?
  [ths {:keys [fn-args] :as stats}]
  (when fn-args
    (let [{:keys [n-args n-string-args] :or {n-args 1 n-string-args 0}} fn-args
          ratio (/ n-string-args (max 1.0 n-args))]
      (and (not (size-detectors/few-functions? ths stats))
           (> ratio (:string-fn-args-ratio ths))))))

(defn- exceeds-code-size?
  [threshold {:keys[active-code-size]}]
  (> active-code-size threshold))

(defn- penalties-for-exceeds-code-size
  [threshold {:keys[active-code-size]}]
  (-> active-code-size
      (- threshold)
      (/ 1000)
      math/round
      (max 2)))

(defn- lines-of-data-declarations-in
  [{:keys [lines-in-file active-code-size] :as _stats}]
  (if (some? lines-in-file)
    (- lines-in-file active-code-size)
    0))

(defn- module-without-any-code?
  [{:keys [active-code-size] :as _stats}]
  (= 0 active-code-size))

(defn- excess-data-declarations?
  "Some hotspots might contain excess amounts of data declarations.
   Typical examples include large enumerations or string constants. These files could probably benefit
   from being split into named segments for each category of constants (e.g. LoginResources.cs, WorkflowResources.cs),
   but we might have some nastier issues that we catch with this biomarker: in Visual Basic, code may contain
   declarations inserted as XML. The Roslyn codebase makes excess use of this, and we want to report it."
  [{:keys [large-data-declarations-delta-loc enable-large-data-declarations]} {:keys [lines-in-file] :as stats}]
  (when (and enable-large-data-declarations
             (some? lines-in-file))
    (let [loc-except-known-functions (lines-of-data-declarations-in stats)]
      (> loc-except-known-functions large-data-declarations-delta-loc))))

(defn- too-many-functions?
  [ths {:keys [n-functions] :as _details}]
  (> n-functions (:many-functions ths)))

(defn- large-file?
  [{:keys [large-file-loc] :as _ths} {:keys[active-code-size]}]
  (> active-code-size large-file-loc))

(defn- many-functions?
  [ths details]
  (and (large-file? ths details)
       (too-many-functions? ths details)))

(defn- low-cohesion?
  [{:keys [lcom4-low-cohesion]} lcom4]
  (>= lcom4 lcom4-low-cohesion))

(defn- penalties-for-low-cohesion
  [lcom4]
  (cond
    (< lcom4 3) 2
    (< lcom4 10) 4
    (< lcom4 15) 5
    :else 7))

(defn- penalty-points-for
  [file-name
   {:keys [really-large-file-loc gigantic-file-loc] :as ths}
   lcom4
   stats]
  (->> 0
       ;               Penalty reason                   Criteria                                                  Penalty points
       (penalize-when "Large file"                      (partial large-file? ths stats)                           (constantly 2))
       (penalize-when "Really large file"               (partial exceeds-code-size? really-large-file-loc stats)  (constantly 2))
       (penalize-when "Gigantic file"                   (partial exceeds-code-size? gigantic-file-loc stats)      (partial penalties-for-exceeds-code-size gigantic-file-loc stats))
       (penalize-when "Excess data declarations"        (partial excess-data-declarations? ths stats))
       (penalize-when "Too many functions"              (partial many-functions? ths stats))
       (penalize-when "Low Cohesion"                    (partial low-cohesion? ths lcom4)                          (partial penalties-for-low-cohesion lcom4))
       (penalize-when "High degree of duplication"      (partial high-degree-of-duplication? ths stats)            (constantly 2))
       (penalize-when "Some code clones"                (partial some-code-clones? ths stats)                      (partial penalties-for-code-clones file-name stats))
       (penalize-when "Large functions in general"      (partial large-functions-in-general? ths stats)            (constantly 2))
       (penalize-when "God function exists"             (partial god-function-exists? ths stats)                   (partial penalties-for-god-function ths stats))
       (penalize-when "Dear God function exists"        (partial dear-god-function-exists? ths stats)              (constantly 9))
       (penalize-when "Many conditionals"               (partial many-conditionals? ths stats)                     (partial many-conditionals-penalties-for ths stats))
       (penalize-when "Many global conditionals"        (partial many-global-conditionals? ths stats)              (partial many-global-conditionals-penalties-for stats))
       (penalize-when "Deep, nested complexity"         (partial nested-logic? ths stats)                          (partial nested-logic-penalties-for ths stats))
       (penalize-when "Deep, global nested complexity " (partial deep-global-nested-logic? ths stats)              (partial deep-global-nesteing-penalties-for stats))
       (penalize-when "Missing arguments abstractions"  (partial missing-fn-arg-abstraction? ths stats))
       (penalize-when "Primitive obsession"             (partial primitive-obsession? ths stats))
       (penalize-when "Any excess argument count"       (partial any-excess-arguments? ths stats))
       (penalize-when "Constructor over-injection"      (partial constructor-over-injection? ths stats))
       (penalize-when "String heavy arguments"          (partial string-heavy-arguments? ths stats))

       ; Test smells
       (penalize-when "Large assertion blocks"          (partial assertion-smells/large-assertion-blocks? ths stats) (partial assertion-smells/penalties-for-large-assertion-blocks stats))
       (penalize-when "Duplicated assertion blocks"     (partial assertion-smells/duplicated-assertion-blocks? ths stats) (partial assertion-smells/penalties-for-duplicated-assertion-blocks stats))))

; a simple conversion table from penalty points to the externally visible score
(def ^:private points->category
  [{:penalities 60 :score 1}
   {:penalities 40 :score 2}
   {:penalities 33 :score 3}
   {:penalities 25 :score 4}
   {:penalities 19 :score 5}
   {:penalities 16 :score 6}
   {:penalities 9 :score 7}
   {:penalities 6 :score 8}
   {:penalities 4 :score 9}
   {:penalities 1 :score 10}
   {:penalities 0 :score 10}])

(defn categorize
  [p]
  (if-let [c (->> points->category
                  (take-while (comp (partial <= p) :penalities))
                  reverse
                  first
                  :score)]
    c
    1)) ; no match -- lowest code health

(defn score-based-on
  [file-name ths lcom4 stats]
  (if (module-without-any-code? stats)
    no-score
    (let [p (penalty-points-for file-name ths lcom4 stats)
          external-score (categorize p)]
      (log/debug "Scoring " file-name ": " p " penalties as a code health of " external-score)
      external-score)))

(s/fdef score-based-on
        :args (s/cat :file-path string?
                     :ths ::biomarkers-spec/ths
                     :lcom4 ::biomarkers-spec/lcom4-value
                     :stats ::biomarkers-spec/code-properties)
        :ret ::biomarkers-spec/code-score)

(def ^:const supported-file-extension-types
  #{".java"
    ".groovy"

    ".cs"
    ".vb"

    ".c"
    ".h"
    ".cc"
    ".cpp"
    ".cxx"
    ".hh"
    ".hpp"
    ".hxx"

    ".efx"
    ".emx"

    ".erl"

    ".go"

    ".kt"

    ".m"
    ".mm"

    ".pl"
    ".pm"

    ".php"

    ".rb"

    ".swift"

    ".js"
    ".ts"
    ".jsx"
    ".tsx"
    ".vue"

    ".scala"

    ".py"})

(defn supports-file-type?
  [file-path]
  (->> file-path loco/extension-of supported-file-extension-types))

(defn- large-enough-for-cohesion-calculations?
  [ths active-code-size]
  (> active-code-size (:minimum-loc-for-cohesion-calculation ths)))

(defn- should-calculate-cohesion?
  [ths active-code-size file-path]
  (and (cohesion/supports-file-type? file-path)
       (large-enough-for-cohesion-calculations? ths active-code-size)))

;; We deliver a perfect cohesion score for small units to avoid false positives.
;; We also go with a perfect cohesional score in case we don't have LCOM4 support
;; for a specific language but still want some biomarkers.
(def ^:private default-cohesion-value 1)

(defn- calculate-cohesion-when-applicable
  [ths {:keys [active-code-size]} file-path input]
  (if (should-calculate-cohesion? ths active-code-size file-path)
    (cohesion/lcom4-value-of file-path input)
    default-cohesion-value))

(defn score
  [file-path
   project
    {:keys [congestion-score-fn historic-trend-score-fn ownership-fn] :as _dynamic-score-fns}
   input]
  (let [ths (language-rules/rules-for file-path)
        code-stats (code-properties/of project file-path input ths)
        congestion (congestion-score-fn)
        lcom4 (calculate-cohesion-when-applicable ths code-stats file-path input)
        details (merge {:cohesion lcom4}
                       {:congestion congestion}
                       code-stats)
        historic-delta (historic-trend-score-fn details)
        ownership (ownership-fn file-path)]
    {:name file-path
     :score (score-based-on file-path ths lcom4 code-stats)
     :details (merge details historic-delta ownership)}))

;; An API that interprets biomarkers, together with specific recommendations.
;;

(def ^:const indication-good 1)
(def ^:const indication-warning 2)
(def ^:const indication-problem 3)

(defn- indication-result-matching
  [is]
  (some (fn [[g r]] (if (g) r)) is))

(defn- ratio->rounded-string
  [r]
  (format "%.2f" r))

(defn- ratio->percentage
  [r]
  (Math/round (float (* r 100))))

(defn- complexity-details-from
  [{:keys [cc-mean cc-total]}]
  (str "The average function complexity is " (ratio->rounded-string cc-mean) ", and the total complexity in the file is " cc-total " (McCabe)"))

(defn- interpret-complexity
  [ths {:keys [cc-mean cc-total] :as cc-details}]
  (let [interpretations
        [[#(and (> 3.0 cc-mean)
                (> 100 cc-total)
                ; guards to avoid presenting potentially conflicting information in our interpretations:
                (not (god-function-exists? ths cc-details))
                (not (nested-logic? ths cc-details))
                (not (many-global-conditionals? ths cc-details))
                (not (excess-data-declarations? ths cc-details)))
          {:title "Low Overall Code Complexity"
           :description(complexity-details-from cc-details)
           :indication indication-good}]
         [#(and (< 7.0 cc-mean)
                (< 300 cc-total))
          {:title "High Overall Code Complexity"
           :description (complexity-details-from cc-details)
           :indication indication-warning}]
         [#(and (more-than-one-function? ths cc-details)
                (many-conditionals? ths cc-details))
          {:title "Many Conditionals"
           :description (complexity-details-from cc-details)
           :indication indication-warning}]]]
    (indication-result-matching interpretations)))

(defn- report-global-complexity
  [ths details]
  (indication-result-matching [[(partial many-global-conditionals? ths details)
                                {:title "Global Complexity"
                                 :description (str "There's global code outside of functions that contain complex constructs. "
                                                   "Consider to encapsulate those business rules in named functions.")
                                 :indication indication-warning}]]))

(defn- report-global-nested-complexity
  [ths {:keys [nested] :as details}]
  (indication-result-matching [[(partial deep-global-nested-logic? ths details)
                                {:title "Global Nested Complexity"
                                 :description (str "There's global code outside of functions that contain deep, nested complexity ("
                                                   (get nested :max-nested-in-global-scope "<unknown>")
                                                   " levels).")
                                 :indication indication-warning}]]))

(defn- god-function-details-from
  [{:keys [cc-max cc-max-name longest-fn-loc longest-fn-loc-name]}]
  (if (= cc-max-name longest-fn-loc-name)
    (str "The function " cc-max-name " has a McCabe complexity of " cc-max " with " longest-fn-loc " lines of code.")
    (str "The function " cc-max-name " has a McCabe complexity of " cc-max ", and the longest function (" longest-fn-loc-name ") has " longest-fn-loc " lines of code.")))

(defn- find-god-function
  [ths cc-details]
  (let [interpretations
        [[(partial dear-god-function-exists? ths cc-details)
          {:title "Large Brain Method Detected"
           :description (god-function-details-from cc-details)
           :indication indication-problem}]

         [(partial god-function-exists? ths cc-details)
          {:title "Brain Method Detected"
           :description(god-function-details-from cc-details)
           :indication indication-warning}]]]
    (indication-result-matching interpretations)))

(defn- cohesion-details-from
  [v]
  (if (< v 10)
    (str "The module seems to have at least " v " different responsibilities.")
    (str "The module seems to have more than 10 different responsibilities.")))

(defn- report-cohesion
  [file-name {:keys [lcom4-suspected-low-cohesion lcom4-low-cohesion] :as ths} {:keys [cohesion active-code-size] :as stats}]
  (when (should-calculate-cohesion? ths active-code-size file-name)
    (let [interpretations
          [[#(and (= 1 cohesion)
                  (not (god-function-exists? ths stats)))
            {:title "Good Cohesion"
             :description "The module seems to consist of related functions and methods."
             :indication indication-good}]
           [#(and (>= lcom4-suspected-low-cohesion cohesion)
                  (< 1 cohesion))
            {:title "Potentially Low Cohesion"
             :description (cohesion-details-from cohesion)
             :indication indication-warning}]
           [#(and (> cohesion lcom4-low-cohesion)
                  (< 1 cohesion))
            {:title "Low Cohesion"
             :description (cohesion-details-from cohesion)
             :indication indication-problem}]]]
      (indication-result-matching interpretations))))

(defn- duplication-details-from
  [v]
  (str "The module contains " v " functions with similar structure."))

(defn- report-duplication
  [ths {:keys [active-code-size n-clones] :as details}]
  (let [interpretations
        [[(partial high-degree-of-duplication? ths details)
          {:title       "High Degree of Code Duplication"
           :description "Many functions are similar and can probably be expressed using shared abstractions."
           :indication  indication-warning}]
         [#(and (< (:small-file-loc ths) active-code-size)
                (< (:many-code-clones ths) n-clones))
          {:title       "Duplicated Code"
           :description (duplication-details-from n-clones)
           :indication  indication-warning}]
         [(partial some-code-clones? ths details)
          {:title       "Duplicated Function Blocks"
           :description (duplication-details-from n-clones)
           :indication  indication-warning}]]]
    (indication-result-matching interpretations)))

(defn- report-large-functions
  [ths details]
  (indication-result-matching [[(partial large-functions-in-general? ths details)
                                {:title "Large Functions"
                                 :description (str "The median length of the functions in this module is "
                                                   (:median-fn-loc details)
                                                   ", which indicates a lack of low-level modularity.")
                                 :indication indication-warning}]]))

(defn- report-excess-function-arguments
  [ths {:keys [fn-args] :as details}]
  (indication-result-matching [[(partial any-excess-arguments? ths details)
                                {:title "Excess function arguments"
                                 :description (str "The function " (:max-arg-name fn-args) " has " (:max-args fn-args) " arguments, which indicates either low cohesion or a missing abstraction that encapsulates those arguments.")
                                 :indication indication-warning}]]))

(defn- report-constructor-over-injection
  [ths {:keys [fn-args] :as details}]
  (indication-result-matching [[(partial constructor-over-injection? ths details)
                                {:title "Constructor Over-Injection"
                                 :description (str "The constructor " (:max-ctor-arg-name fn-args) " has " (:max-ctor-args fn-args) " arguments, which suggests low cohesion or a missing abstraction that encapsulates a higher-level concept.")
                                 :indication indication-warning}]]))

(defn- report-primitive-obsession
  [ths details]
  (indication-result-matching [[(partial primitive-obsession? ths details)
                                {:title "Primitive obsession"
                                 :description
                                        (str "A high degree of the functions (" (ratio->percentage (primitive-args-ratio-from (:fn-args details))) " %) have primitive types as arguments, which hints at a missing domain language.")
                                 :indication indication-warning}]]))

(defn- report-missing-function-arg-abstraction
  [ths details]
  (indication-result-matching [[(partial missing-fn-arg-abstraction? ths details)
                                {:title "Missing function argument abstraction"
                                 :description "A high degree of the functions have plenty of arguments, which indicates missing abstractions that encapsulate related data."
                                 :indication indication-warning}]]))

(defn- report-string-heavy-functions
  [ths details]
  (indication-result-matching [[(partial string-heavy-arguments? ths details)
                                {:title "Heavy usage of string arguments"
                                 :description "String is a generic type that often fail to capture the constraints of the domain object it represents. Consider introducing custom types instead."
                                 :indication indication-warning}]]))

;; Indicators for trends over the past month
;;

(defn- complex-code-in-current-snapshot?
  [ths {:keys [cc-mean]}]
  (> cc-mean (:cc-mean ths)))

(defn- grows-in-complexity?
  [{:keys [complexity-delta-in-cc-for-trend-warning] :as ths}
   {:keys [delta] :as details}]
  (when (and (some? delta)
             (complex-code-in-current-snapshot? ths details))
    (let [growth (:cc-total-delta delta)]
      (<= complexity-delta-in-cc-for-trend-warning growth))))

(defn- decreases-in-complexity?
  [{:keys [complexity-delta-in-cc-for-trend-warning] :as _ths}
   {:keys [delta] :as _details}]
  (when (some? delta)
    (let [growth (:cc-total-delta delta)]
      (when (< growth 0)
        (<= complexity-delta-in-cc-for-trend-warning (Math/abs growth))))))

(defn- complex-code-by-former-contributors?
  [score
   {:keys [knowledge-loss-for-former-contributors-warning] :as _ths}
   {:keys [knowledge-loss] :as social}]
  (when (and (some? social)
             (>= bad-health score))
      (<= knowledge-loss-for-former-contributors-warning knowledge-loss)))

(defn- complex-code-by-one-developer?
  [score
   {:keys [ownership-for-one-developer-warning] :as ths}
   {:keys [ownership] :as social}]
  (when (and (some? social)
             (>= bad-health score)
             (not (complex-code-by-former-contributors? score ths social)))
    (<= ownership-for-one-developer-warning ownership)))

(defn- report-increasing-complexity-trend
  [ths {:keys [delta] :as details}]
  (indication-result-matching [[(partial grows-in-complexity? ths details)
                                {:title "Complexity Trend Growth"
                                 :description
                                        (str "The code complexity has increased with "
                                             (:cc-total-delta delta) " conditionals over the past month. Ensure that the module isn't a growing problem.")
                                 :indication indication-warning}]]))

(defn- report-decreasing-complexity-trend
  [ths {:keys [delta] :as details}]
  (indication-result-matching [[(partial decreases-in-complexity? ths details)
                                {:title "Complexity Trend Decrease"
                                 :description
                                        (str "The code complexity has decreased with "
                                             (Math/abs (get delta :cc-total-delta 0)) " conditionals over the past month. Well done!")
                                 :indication indication-good}]]))

(defn- report-complex-code-by-former-contributors
  [score ths {:keys [social] :as _details}]
  (indication-result-matching [[(partial complex-code-by-former-contributors? score ths social)
                                {:title "Complex Code By Former Contributors"
                                 :description
                                        (str "The developers responsible for "
                                             (Math/round (* 100.0 (get social :knowledge-loss 0)))
                                             "% of the code are no longer part of the project. This increases the maintenance risk. Prioritize a refactoring?")
                                 :indication indication-warning}]]))

(defn- report-complex-code-by-one-developer
  [score ths {:keys [social] :as _details}]
  (indication-result-matching [[(partial complex-code-by-one-developer? score ths social)
                                {:title "Knowledge Island In Complex Hotspots"
                                 :description
                                        (str "A single developer ("
                                             (get social :owner "[unknown]") ") has written "
                                             (Math/round (* 100.0 (get social :ownership 0)))
                                             "% of the code. This increases the maintenance risk as others might find it hard to work on this code. Prioritize a refactoring?")
                                 :indication indication-warning}]]))

;; Code Comment Patterns
;;

(defn- matched-code-comment-statistics
  [code-comment-match]
  (some->>
    code-comment-match
    (filter (fn at-least-one-match? [{:keys [n-matches]}] (< 0 n-matches)))))

(defn- code-comment-indication-title
  [code-comment-match]
  (get code-comment-match :pattern-name "Code Comments (unconfigured pattern name):"))

(defn- report-code-comment-patterns
  "The user can configure certain expressions that we will match against the comments
   in the code. Typical usages include looking for TODOs or static code analyisis supressions.
   This is an optional biomarker, and the result can result in *multiple* indications which deviates
   from the other biomarkers."
  [_ths {:keys [code-comment-match] :as _details}]
  (let [ms (matched-code-comment-statistics code-comment-match)]
    (when-not (empty? ms)
      (map (fn [m]
             {:title (code-comment-indication-title m)
              :description
                     (str "You have "
                          (get m :n-matches "[unknown]") " occourences of the pattern "
                          (get m :pattern-name "[unconfigured pattern name]")
                          " in the source code file.")
              :indication indication-warning})
           ms))))

;; NOTE: developer congestion does NOT add to the scoring of the file, but we do
;; keep it as an indication. The reason for this is because it's hard to tell
;; if it's a problem or not without more context (e.g. popular open source?, prefered practices).
(defn- developer-congestion?
  [{:keys [fractal-value-for-congestion many-authors]}
   {:keys [authors fractal-value] :as congestion}]
  (and (some? congestion)
       (< many-authors authors)
       (< fractal-value-for-congestion fractal-value)))

(defn- report-developer-congestion
  [ths {:keys [congestion] :as _details}]
  (indication-result-matching [[(partial developer-congestion? ths congestion)
                                {:title "Developer Congestion"
                                 :description
                                        (str "There are many developers (" (:authors congestion) ") working on the module. This might put you at risk for high coordination costs and defects.")
                                 :indication indication-warning}]]))

(defn- report-excess-data-declarations
  [ths details]
  (indication-result-matching [[(partial excess-data-declarations? ths details)
                                {:title "Excess Data Declarations"
                                 :description
                                        (str "We found "
                                             (lines-of-data-declarations-in details)
                                             " lines of code that look like data declarations. "
                                             "Perhaps that part of the module needs a higher-level structure?")
                                 :indication indication-warning}]]))

(defn- report-nested-logic
  [ths {:keys [nested] :as details}]
  (indication-result-matching [[(partial nested-logic? ths details)
                                {:title "Deeply Nested Logic"
                                 :description (let [{:keys [max-nested-complexity-depth
                                                            max-nested-complexity-depth-name
                                                            nested-complexity-of-interest]} nested
                                                    n-complexity-islands (count nested-complexity-of-interest)]
                                                (if (<= 2 n-complexity-islands)
                                                  (str "The function " max-nested-complexity-depth-name " has a nested conditional depth of "
                                                      max-nested-complexity-depth ". In addition, there are " (dec n-complexity-islands)
                                                       " other functions with deep conditional logic. Try to extract those nested conditions into named functions.")
                                                  (str "The function " max-nested-complexity-depth-name " has a nested conditional depth of "
                                                       max-nested-complexity-depth ". This makes it hard to follow the overall algorithm of the " max-nested-complexity-depth-name " function.")))
                                 :indication indication-problem}]]))

(defn- brain-class?
  [ths details]
  (and (god-function-exists? ths details)
       (large-file? ths details)
       (too-many-functions? ths details)))

(defn- report-class-design-issues
  "This is just and indication and doesn't add to the overall score directly, although some of the underlaying
   metrics contribute to the score (Brain Method, Large File, etc)."
  [ths details]
  (indication-result-matching [[(partial brain-class? ths details)
                                {:title "Brain Class"
                                 :description
                                        (str "This class seems to accumulate an excessive amount of behavior, with both a Brain Method and " (:n-functions details) " methods overall. Look for opportunities to modularize the design.")
                                 :indication indication-problem}]
                               [(partial too-many-functions? ths details)
                                {:title "Many Methods"
                                 :description
                                        (str "This class has a total of " (:n-functions details) " methods, which put it at risk of evolving into a Brain Class. Look to modularize the design before it becomes a problem.")
                                 :indication indication-warning}]]))

(defn- report-large-assertion-blocks
  [ths details]
  (indication-result-matching [[(partial assertion-smells/large-assertion-blocks? ths details)
                                {:title "Large Assertion Blocks"
                                 :description (assertion-smells/describe-large-assertion-blocks details)
                                 :indication indication-warning}]]))

(defn- report-duplicated-assertion-blocks
  [ths details]
  (indication-result-matching [[(partial assertion-smells/duplicated-assertion-blocks? ths details)
                                {:title "Duplicated Assertion Blocks"
                                 :description (assertion-smells/describe-duplkicated-assertion-blocks details)
                                 :indication indication-warning}]]))

(s/def ::details ::biomarkers-spec/code-properties)

(s/def ::markers (s/keys :req-un [::score ::name ::details]))

(s/def ::indication (s/and nat-int? #{indication-good indication-warning indication-problem}))

(s/def ::interpretation (s/keys :req-un [::title ::description ::indication]))

(defn interpret-for-a-human
  [markers]
  (let [ds (:details markers)
        file-name (:name markers)
        score (:score markers)
        ths (language-rules/rules-for file-name)
        user-configured-indicators (report-code-comment-patterns ths ds)
        dynamic-results
        (->> [interpret-complexity
              report-global-complexity
              report-excess-data-declarations
              report-nested-logic
              report-global-nested-complexity
              find-god-function
              (partial report-cohesion file-name)
              report-duplication
              report-excess-function-arguments
              report-constructor-over-injection
              report-primitive-obsession
              report-missing-function-arg-abstraction
              report-string-heavy-functions
              report-large-functions
              report-class-design-issues
              report-developer-congestion
              report-large-assertion-blocks
              report-duplicated-assertion-blocks
              report-increasing-complexity-trend
              report-decreasing-complexity-trend
              (partial report-complex-code-by-former-contributors score)
              (partial report-complex-code-by-one-developer score)]
             (map (fn [i] (i ths ds)))
             (filter some?))]
    (->> (concat dynamic-results user-configured-indicators)
         (sort-by :indication)
         reverse)))

(s/fdef interpret-for-a-human
        :args (s/cat :markers ::markers)
        :ret (s/coll-of ::interpretation))

