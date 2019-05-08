(ns hotspots-x-ray.recommendations.cohesion
  (:require [taoensso.timbre :as timbre]
            [hotspots-x-ray.languages.antlr :as antlr]
            [hotspots-x-ray.diagnostics.performance :as performance]
            [hotspots-x-ray.recommendations.function-body-parsing :as function-body-parsing]
            [hotspots-x-ray.recommendations.language-specific-biomarker-rules :as language-specific-rules]
            [hotspots-x-ray.languages.special-case-parsers.rsa-rte-model-parser :as rsa-rte-parser]
            [hotspots-x-ray.recommendations.code-statistics :as code-statistics]
            [evolutionary-metrics.complexity.loco :as loco])
  (:import (hotspots_x_ray.languages.cohesion JavaCohesionPropertiesListener
                                              CSharpCohesionPropertiesListener
                                              Python3CohesionPropertiesListener
                                              SwiftCohesionPropertiesListener
                                              VisualBasicCohesionPropertiesListener
                                              GoCohesionPropertiesListener
                                              PerlCohesionPropertiesListener
                                              PHPCohesionPropertiesListener
                                              RubyCohesionPropertiesListener
                                              KotlinCohesionPropertiesListener
                                              ScalaCohesionPropertiesListener
                                              ErlangCohesionPropertiesListener)
           (hotspots_x_ray.languages.generated JavaCohesionLexer JavaCohesionParser
                                               CSharpCohesionLexer CSharpCohesionParser
                                               Python3Lexer Python3Parser
                                               SwiftCohesionLexer SwiftCohesionParser
                                               VisualBasicLexer VisualBasicParser
                                               GoLexer GoParser
                                               PerlLexer PerlParser
                                               PHPLexer PHPParser
                                               RubyMicroLexer RubyMicroParser
                                               KotlinLexer KotlinParser
                                               ScalaLexer ScalaParser
                                               ErlangLexer ErlangParser)
           (java.text ParseException)
           (java.util.regex PatternSyntaxException)))

(defn- parse-results->cohesion-properties
  [listener]
  {:fields (.getFields listener)
   :functions (antlr/parsed-fns->internal-format (.getFunctions listener))})

(def ^:private perl-parser
  (antlr/make-parser {:lexer-ctor    #(PerlLexer. %1)
                      :parser-ctor   #(PerlParser. %1)
                      :context-fn    #(.translationunit %1)
                      :listener-ctor #(PerlCohesionPropertiesListener.)}))

(def ^:private cohesion-properties-parsers
  {".java"  (antlr/make-parser {:lexer-ctor    #(JavaCohesionLexer. %1)
                                      :parser-ctor   #(JavaCohesionParser. %1)
                                      :context-fn    #(.translationunit %1)
                                      :listener-ctor #(JavaCohesionPropertiesListener.)})

   ".cs"    (antlr/make-parser {:lexer-ctor    #(CSharpCohesionLexer. %1)
                                      :parser-ctor   #(CSharpCohesionParser. %1)
                                      :context-fn    #(.translationunit %1)
                                      :listener-ctor #(CSharpCohesionPropertiesListener.)})

   ".erl"   (antlr/make-parser {:lexer-ctor    #(ErlangLexer. %1)
                                :parser-ctor   #(ErlangParser. %1)
                                :context-fn    #(.forms %1)
                                :listener-ctor #(ErlangCohesionPropertiesListener.)})

   ".kt"    (antlr/make-parser {:lexer-ctor    #(KotlinLexer. %1)
                                :parser-ctor   #(KotlinParser. %1)
                                :context-fn    #(.translationunit %1)
                                :listener-ctor #(KotlinCohesionPropertiesListener.)})

   ".vb"    (antlr/make-parser {:lexer-ctor    #(VisualBasicLexer. %1)
                                :parser-ctor   #(VisualBasicParser. %1)
                                :context-fn    #(.translationunit %1)
                                :listener-ctor #(VisualBasicCohesionPropertiesListener.)})

   ".swift" (antlr/make-parser {:lexer-ctor    #(SwiftCohesionLexer. %1)
                               :parser-ctor   #(SwiftCohesionParser. %1)
                               :context-fn    #(.translationunit %1)
                               :listener-ctor #(SwiftCohesionPropertiesListener.)})
   ".go"    (antlr/make-parser {:lexer-ctor    #(GoLexer. %1)
                                :parser-ctor   #(GoParser. %1)
                                :context-fn    #(.translationunit %1)
                                :listener-ctor #(GoCohesionPropertiesListener.)})
   ".php"   (antlr/make-parser {:lexer-ctor    #(PHPLexer. %1)
                                :parser-ctor   #(PHPParser. %1)
                                :context-fn    #(.translationunit %1)
                                :listener-ctor #(PHPCohesionPropertiesListener.)})

   ".pl"    perl-parser
   ".pm"    perl-parser

   ".py"    (antlr/make-parser {:lexer-ctor    #(Python3Lexer. %1)
                                :parser-ctor   #(Python3Parser. %1)
                                :context-fn    #(.file_input %1)
                                :listener-ctor #(Python3CohesionPropertiesListener.)})
   ".rb"    (antlr/make-parser {:lexer-ctor    #(RubyMicroLexer. %1)
                                :parser-ctor   #(RubyMicroParser. %1)
                                :context-fn    #(.prog %1)
                                :listener-ctor #(RubyCohesionPropertiesListener.)})
   ".scala" (antlr/make-parser {:lexer-ctor    #(ScalaLexer. %1)
                                :parser-ctor   #(ScalaParser. %1)
                                :context-fn    #(.translationunit %1)
                                :listener-ctor #(ScalaCohesionPropertiesListener.)})})

(defn- field-obj->clj-structures
  [{:keys [fields] :as parse-results}]
  (assoc parse-results
    :fields
    (map (fn [o] (.name o)) fields)))

(defn- parse-normal-languages
  [parser file-path input]
  (->> input
       (parser parse-results->cohesion-properties file-path)
       field-obj->clj-structures))

(def ^:private special-case-parsers
  {".emx" rsa-rte-parser/parse-cohesion-properties
   ".efx" rsa-rte-parser/parse-cohesion-properties})

(defn- parse-special-cases
 [parser file-path input]
 (parser parse-results->cohesion-properties file-path input))

(defn- parser-for
  [file-path]
  (let [ext (loco/extension-of file-path)
        normal-parser (get cohesion-properties-parsers ext)
        special-parser (get special-case-parsers ext)]
    (if (some? normal-parser)
      (partial parse-normal-languages normal-parser)
      (when (some? special-parser)
        (partial parse-special-cases special-parser)))))

(defn supports-file-type?
  [file-path]
  (some? (parser-for file-path)))

(defn extract-cohesion-properties
 [file-path input]
 (let [ext (loco/extension-of file-path)]
   (if-let [parser (parser-for file-path)]
     (try
       (performance/with-timbre-exe-time-info
         (parser file-path input))
       (catch Throwable e
         (do
           (timbre/error "X-Ray failed to calculate the cohesion of the file " file-path e)
            (throw (ParseException. (str "X-Ray failed to calculate the cohesion of the file " file-path) 0)))))
      (throw (Exception. (str "No cohesion algorithm available for file extension " ext
                              ". Supported extensions are: " cohesion-properties-parsers))))))

; NOTE: this is a *very* relaxed heuristic and we need a real parser here.
; With the current implementation we will match literals, e.g. "some_function_name", as
; a function call.
(defn- reference-to?
  [field {:keys [body]}]
  (try
    (let [p (re-pattern (str "(^|[^\\w]|return)" field "[^\\w]"))]
      (re-find p body))
    (catch PatternSyntaxException _e ; ignore everything we cannot match like overloaded arrays and stuff.
      false)))

(defn- php-reference-to?
  [field {:keys [body]}]
  (let [p (re-pattern (str "(^|[^\\w]|return)" field "[^\\w]"))
        direct-match (re-find p body)]
    (if (some? direct-match)
      direct-match
      (let [name-part (clojure.string/replace field #"^[\\$&]+" "")
            p-property (re-pattern (str "(^|[^\\w]|return)" "\\$this->" name-part "[^\\w]"))]
        (re-find p-property body)))))

(defn- python-reference-to?
  "An unfortunate special case as Python has whitespace, no special charaters like (, to
   separate member access and whitespace isn't maintained in the current parse results."
  [field {:keys [body]}]
  (let [p (re-pattern (str field "([^\\w]|$)"))]
    (re-find p body)))

(defn- names-of
  [fns]
  (set (map :name fns)))

(defn- groups-by-field-usage
  [field-ref-fn functions fields]
  (reduce (fn [acc f]
            (let [field-users (filter (partial field-ref-fn f) functions)]
              (timbre/trace "Field " f " used by " (names-of field-users))
              (into acc [(names-of field-users)])))
          []
          fields))

(defn- groups-by-direct-function-calls
  [functions]
  (reduce (fn [acc f]
            (let [n (:name f)
                  callers (filter (partial reference-to? n) functions)
                  grouped (into [f] callers)]
              (timbre/trace "Function " n " called by " (names-of grouped))
              (into acc [(names-of grouped)])))
          []
          functions))

(defn- combine-related-groups
  "By now we have a set of groups that relate to each other by
   means of a single field or function call. Now we need to combine
   functions in separate groups that relate indirectly to each other.
   That is, if the same function occours in different groups, then
   those groups belong together."
  [groups]
  (reduce (fn [acc g]
            (let [rel (filter (comp seq (partial clojure.set/intersection g)) acc)
                  remaining (remove (comp seq (partial clojure.set/intersection g)) acc)
                  joined-rel (reduce clojure.set/union g rel)
                  new-acc (into remaining [joined-rel])]
              (timbre/trace "Scanning combinations " (pr-str acc) " for " (pr-str g) ", found = " (pr-str rel))
              new-acc))
          []
          groups))

(defn- group-connected-functions
  [field-ref-fn fields functions]
  (let [connected-by-field (groups-by-field-usage field-ref-fn functions fields)
        connected-by-call (groups-by-direct-function-calls functions)]
    (combine-related-groups
      (into connected-by-field connected-by-call))))

(defn- language-based-field-detector-from
  [file-path]
  (case (loco/extension-of file-path)
    ".py" python-reference-to?
    ".php" php-reference-to?
    reference-to?))

(defn- small-function-threshold-in-loc-for
  [file-path]
  (let [ext (loco/extension-of file-path)]
    (if (= ext ".erl")
      5 ; Erlang has more verbose function bodies (end. guards, etc).
      3)))

(defn- small-functions-in
  [file-path fns]
  (let [threshold (small-function-threshold-in-loc-for file-path)]
    (->> fns
         (filter #(>= threshold (code-statistics/loc-from %)))
         (map :name)
         set)))

(defn- mandatory-functions-to-exclude
  [file-path]
  (let [{:keys [functions-to-exclude-from-cohesion]} (language-specific-rules/rules-for file-path)]
    (or functions-to-exclude-from-cohesion #{})))

(defn- remove-functions-found-in
  [functions-to-remove functions]
  (map (fn [g] (clojure.set/difference g functions-to-remove)) functions))

(defn connected-function-groups-in
  "Note: We need to consume a potential stream twice. Let the reader provide a function
   that knows how to fetch a fresh stream since it might be just a string in case
   of our unit tests."
  [file-path input]
  (let [{:keys [fields functions]} (extract-cohesion-properties file-path input)
        field-ref-fn (language-based-field-detector-from file-path)
        fn-adapter (function-body-parsing/function-adapter-for (loco/extension-of file-path) input)
        small-functions (small-functions-in file-path functions)
        functions-to-exclude (mandatory-functions-to-exclude file-path)]
    (->> functions
         fn-adapter
         (group-connected-functions field-ref-fn fields)
         (remove-functions-found-in small-functions)
         (remove-functions-found-in functions-to-exclude)
         (filter seq))))

(defn lcom4-value-of
  [file-path input]
  (max 1 (count (connected-function-groups-in file-path input))))
