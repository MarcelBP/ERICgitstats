(ns hotspots-x-ray.recommendations.code-complexity
  (:require [taoensso.timbre :as timbre]
            [hotspots-x-ray.diagnostics.parse-errors :as parse-errors]
            [hotspots-x-ray.diagnostics.performance :as performance]
            [hotspots-x-ray.languages.specs :as specs]
            [hotspots-x-ray.recommendations.code-extraction :as extraction]
            [hotspots-x-ray.recommendations.function-body-parsing :as function-body-parsing]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.complexity.loco :as loco])
  (:import (hotspots_x_ray.diagnostics PersistentParseErrorListener)
           (org.antlr.v4.runtime.tree ParseTreeWalker)
           (org.antlr.v4.runtime CommonTokenStream ANTLRInputStream)
           (hotspots_x_ray.languages JavaComplexityCounter
                                     CSharpComplexityCounter
                                     CStyleComplexityCounter
                                     KotlinComplexityCounter
                                     ECMAScriptComplexityCounter
                                     PLSqlComplexityCounter
                                     ScalaComplexityCounter
                                     PythonComplexityCounter
                                     SwiftComplexityCounter
                                     VisualBasicComplexityCounter
                                     GoComplexityCounter
                                     PHPComplexityCounter
                                     RubyComplexityCounter
                                     PerlComplexityCounter
                                     ErlangComplexityCounter)
           (hotspots_x_ray.languages.generated JavaComplexityParser JavaComplexityLexer
                                               CSharpComplexityParser CSharpComplexityLexer
                                               CStyleComplexityLexer CStyleComplexityParser
                                               KotlinComplexityLexer KotlinComplexityParser
                                               ECMAScriptComplexityLexer ECMAScriptComplexityParser
                                               PLSqlComplexityLexer PLSqlComplexityParser
                                               ScalaComplexityLexer ScalaComplexityParser
                                               Python3Lexer Python3Parser
                                               SwiftComplexityLexer SwiftComplexityParser
                                               VisualBasicComplexityLexer VisualBasicComplexityParser
                                               GoComplexityLexer GoComplexityParser
                                               PHPComplexityLexer PHPComplexityParser
                                               RubyComplexityLexer RubyComplexityParser
                                               PerlComplexityLexer PerlComplexityParser
                                               ErlangLexer ErlangParser)

           (hotspots_x_ray.languages.nestedcomplexity CStyleNestedComplexityCounter
                                                      JavaNestedComplexityCounter
                                                      CSharpNestedComplexityCounter
                                                      GroovyNestedComplexityCounter
                                                      ECMAScriptNestedComplexityCounter
                                                      ScalaNestedComplexityCounter
                                                      ObjectiveCNestedComplexityCounter
                                                      PerlNestedComplexityCounter
                                                      PythonNestedComplexityCounter
                                                      SwiftNestedComplexityCounter
                                                      VisualBasicNestedComplexityCounter
                                                      GoNestedComplexityCounter
                                                      PHPNestedComplexityCounter
                                                      RubyNestedComplexityCounter
                                                      KotlinNestedComplexityCounter
                                                      ErlangNestedComplexityCounter)
           (hotspots_x_ray.languages.generated CStyleNestedComplexityLexer CStyleNestedComplexityParser
                                               JavaNestedComplexityLexer JavaNestedComplexityParser
                                               CSharpNestedComplexityLexer CSharpNestedComplexityParser
                                               GroovyNestedComplexityLexer GroovyNestedComplexityParser
                                               ECMAScriptNestedComplexityLexer ECMAScriptNestedComplexityParser
                                               ScalaNestedComplexityLexer ScalaNestedComplexityParser
                                               ObjectiveCNestedComplexityLexer ObjectiveCNestedComplexityParser
                                               SwiftNestedComplexityLexer SwiftNestedComplexityParser
                                               VisualBasicNestedComplexityLexer VisualBasicNestedComplexityParser
                                               GoNestedComplexityLexer GoNestedComplexityParser
                                               PHPNestedComplexityLexer PHPNestedComplexityParser
                                               RubyNestedComplexityLexer RubyNestedComplexityParser
                                               KotlinNestedComplexityLexer KotlinNestedComplexityParser
                                               PerlNestedComplexityLexer PerlNestedComplexityParser)
           (java.text ParseException)))

(defn- make-antlr-parser
  "Given a map of ANTLR constructors and functions, return a parser function."
  [{:keys [lexer-ctor parser-ctor context-fn listener-ctor]}]
  (fn [file-path input]
    (let [file-stream (ANTLRInputStream. input)
          lexer (lexer-ctor file-stream)
          tokens (CommonTokenStream. lexer)
          parser (parser-ctor tokens)
          walker (ParseTreeWalker.)
          listener (listener-ctor)
          error-listener (PersistentParseErrorListener.)]
      (.removeErrorListeners lexer)
      (.removeErrorListeners parser)
      (.addErrorListener parser error-listener)
      (.walk walker listener (context-fn parser))
      (let [result (max 1 (.complexityValue listener))]
        (parse-errors/trace-potential-errors file-path error-listener)
        (.removeErrorListeners parser)
        result))))

(defmacro parser-for
  [base-name]
  `(make-antlr-parser {:lexer-ctor    #(~(symbol (str base-name "ComplexityLexer.")) %1)
                       :parser-ctor   #(~(symbol (str base-name "ComplexityParser.")) %1)
                       :context-fn    #(.method %)
                       :listener-ctor #(~(symbol (str base-name "ComplexityCounter.")))}))

(def ^:private cstyle-parser (parser-for "CStyle"))

(def ^:private sql-parser (parser-for "PLSql"))

(def ^:private perl-parser (parser-for "Perl"))

(def ^:private ecma-script-parser (parser-for "ECMAScript"))

(def ^:private python-parser
  (make-antlr-parser {:lexer-ctor    #(Python3Lexer. %1)
                      :parser-ctor   #(Python3Parser. %1)
                      :context-fn    #(.mc_cabe_complexity_entry_point %1)
                      :listener-ctor #(PythonComplexityCounter.)}))

(def ^:private erlang-parser
  (make-antlr-parser {:lexer-ctor    #(ErlangLexer. %1)
                      :parser-ctor   #(ErlangParser. %1)
                      :context-fn    #(.forms %1)
                      :listener-ctor #(ErlangComplexityCounter.)}))

(def ^:private complexity-parsers
  {".java"   (parser-for "Java")
   ".groovy" (parser-for "Java")
   ".cs"     (parser-for "CSharp")
   ".c"       cstyle-parser
   ".h"       cstyle-parser
   ".cc"      cstyle-parser
   ".cpp"     cstyle-parser
   ".cxx"     cstyle-parser
   ".hh"      cstyle-parser
   ".hpp"     cstyle-parser
   ".hxx"     cstyle-parser
   ".m"       cstyle-parser
   ".mm"      cstyle-parser
   ".kt"     (parser-for "Kotlin")

   ".erl"     erlang-parser

   ;; The RSA-RTE models are parsed into snippets of C++ functions so we can just
   ;; re-use the existing cyclomatic complexity parsers here.
   ".emx"     cstyle-parser
   ".efx"     cstyle-parser

   ".go"     (parser-for "Go")

   ".php"    (parser-for "PHP")

   ".pl"    perl-parser
   ".pm"    perl-parser

   ".py"     python-parser

   ".rb"    (parser-for "Ruby")

   ".vb"     (parser-for "VisualBasic")

   ".js"     ecma-script-parser
   ".ts"     ecma-script-parser
   ".jsx"    ecma-script-parser
   ".tsx"    ecma-script-parser
   ".vue"    ecma-script-parser

   ".swift"  (parser-for "Swift")

   ".sql"    sql-parser
   ".psql"   sql-parser
   ".plsql"  sql-parser
   ".prc"    sql-parser
   ".fnc"    sql-parser
   ".bdy"    sql-parser
   ".scala"  (parser-for "Scala")})

(def ^:const unknown-complexity-result "-")

(defn function-level
  [file-path input]
  (let [ext (loco/extension-of file-path)]
    (if-let [parser (get complexity-parsers ext)]
      (try
        (performance/with-timbre-exe-time-info
          (str (parser file-path input)))
        (catch Throwable e
          (do
            (timbre/error "X-Ray complexity failed to parse the file " file-path e)
            (throw (ParseException. (str "X-Ray complexity failed to parse " file-path) 0)))))
      unknown-complexity-result)))


(defn- cyclomatic-complexity-for
  [parser {:keys [body] :as fn-stat}]
  (assoc fn-stat :cc (str (parser body))))

(s/def ::filename string?)

(s/def ::cc (s/and string? (s/or
                             :no-complexity (partial = "-")
                             :complexity (partial re-matches #"\d+"))))

(s/def ::function-complexity (s/keys :req-un [::specs/name ::specs/start-line ::specs/end-line ::cc]))

(s/def ::function-complexity-statistics (s/coll-of ::function-complexity))

(defn by-functions-in-file
  "Splits the file content per function, as defined by the parse results, to caluclate
   a cyclomatic complexity value while maintaining white space separators. This is a
   necessity in order to avoid false positives (e.g. without white space we don't know
   that returnSomethingFromHere is a method call rather than the return token)."
  [file-path lines-in-file functions]
  (let [ext (loco/extension-of file-path)
        parser (get complexity-parsers ext)]
    (if parser
      (try
        (performance/with-timbre-exe-time-info
          (->> functions
               (extraction/code-by-function-name lines-in-file)
               (map (partial cyclomatic-complexity-for (partial parser file-path)))))
        (catch Throwable e
          (do
            (timbre/error "X-Ray complexity failed to parse the file " file-path e)
            (throw (ParseException. (str "X-Ray complexity calculator failed to parse " file-path) 0)))))
      (map (fn [s] (assoc s :cc unknown-complexity-result)) functions))))

(def ^:private languages-with-complexity-in-global-scope
  #{".php" ".py" ".rb" ".kt" ".pl" ".pm"})

(def ^:private low-default-complexity 0)

(defn total-in-file
  "Some languages (e.g. PHP) might have logic in the main body of a file, that is, outside
   of functions. We want to catch complex parts there as well."
  [file-path input]
  (let [ext (loco/extension-of file-path)
        parser (get complexity-parsers ext)]
    (if (and (some? parser)
             (languages-with-complexity-in-global-scope ext))
      (try
        (performance/with-timbre-exe-time-info
          (parser file-path input))
        (catch Throwable e
          (do
            (timbre/error "X-Ray total complexity failed to parse the file " file-path e)
            (throw (ParseException. (str "X-Ray total complexity calculator failed to parse " file-path) 0)))))
      low-default-complexity))) ; low default complexity

(s/fdef by-functions-in-file
        :args (s/cat :file-path ::filename
                     :lines-in-file (s/coll-of string?)
                     :functions ::specs/parsed-function-statistics)
        :ret ::function-complexity-statistics)

(def ^:private cstyle-nested-parser (parser-for "CStyleNested"))

(def ^:private objective-c-nested-parser (parser-for "ObjectiveCNested"))

(def ^:private ecma-script-nested-parser (parser-for "ECMAScriptNested"))

(def ^:private perl-nested-parser (parser-for "PerlNested"))

(def ^:private erlang-nested-parser
  (make-antlr-parser {:lexer-ctor    #(ErlangLexer. %1)
                      :parser-ctor   #(ErlangParser. %1)
                      :context-fn    #(.forms %)
                      :listener-ctor #(ErlangNestedComplexityCounter.)}))

;; Python is a special case as we need to maintain its indentation.
;; I've tried several approaches, but finally fell back to parsing the
;; whole file in one sweep and then map the results back to the functions
;; we already know about. I know, this isn't optimal, but I haven't found
;; a simpler way.

(defn- parse-nested-python
  [file-path input]
  (let [file-stream (ANTLRInputStream. input)
        lexer (Python3Lexer. file-stream)
        tokens (CommonTokenStream. lexer)
        parser (Python3Parser. tokens)
        walker (ParseTreeWalker.)
        listener (PythonNestedComplexityCounter.)
        error-listener (PersistentParseErrorListener.)]
    (.removeErrorListeners lexer)
    (.removeErrorListeners parser)
    (.addErrorListener parser error-listener)
    (.walk walker listener (.file_input parser))
    (let [result (.functionsWithDepth listener)]
      (parse-errors/trace-potential-errors file-path error-listener)
      (.removeErrorListeners parser)
      result)))

(defn parse-nested-python-complexity-for
  [file-path input functions]
  (let [fns-with-depth (parse-nested-python file-path input)]
    (map (fn [{:keys [name] :as v}]
           (assoc v :cc (str (get fns-with-depth name unknown-complexity-result))))
         functions)))

(def ^:private nested-complexity-parsers
  {".c"       cstyle-nested-parser
   ".h"       cstyle-nested-parser
   ".cc"      cstyle-nested-parser
   ".cpp"     cstyle-nested-parser
   ".cxx"     cstyle-nested-parser
   ".hh"      cstyle-nested-parser
   ".hpp"     cstyle-nested-parser
   ".hxx"     cstyle-nested-parser

   ;; The RSA-RTE models are parsed into snippets of C++ functions so we can just
   ;; re-use the existing cyclomatic complexity parsers here.
   ".emx"     cstyle-nested-parser
   ".efx"     cstyle-nested-parser

   ".erl"     erlang-nested-parser

   ".kt"     (parser-for "KotlinNested")

   ".m"       objective-c-nested-parser
   ".mm"      objective-c-nested-parser

   ".swift"  (parser-for "SwiftNested")

   ".go"     (parser-for "GoNested")

   ".php"     (parser-for "PHPNested")

   ".pl"    perl-nested-parser
   ".pm"    perl-nested-parser

   ".rb"    (parser-for "RubyNested")

   ;; NOTE: Python is a special case that's handled separately
   ;; ".py"  special-parser

   ".js"     ecma-script-nested-parser
   ".ts"     ecma-script-nested-parser
   ".jsx"    ecma-script-nested-parser
   ".tsx"    ecma-script-nested-parser
   ".vue"    ecma-script-nested-parser

   ".cs"     (parser-for "CSharpNested")
   ".vb"     (parser-for "VisualBasicNested")

   ".java"    (parser-for "JavaNested")
   ".groovy"  (parser-for "GroovyNested")
   ".scala"   (parser-for "ScalaNested")})

;; Alright, this is a bit more tricky than I'd like it: We have a set
;; of languages (e.g. Go, Python) where whitespace is significant in order
;; to parse out complexity constructs. PHP falls in the same category, however
;; we only want to treat is special when calculating complexity as the cohesion
;; parsing algorithms works out of the box. I'm note sure why for the moment so
;; let's stick with the following until I find out.
(def ^:private special-cases-requiring-line-separators #{".php"})

(defn- custom-complexity-for-functions-in-file
  [file-path input available-parsers functions]
  (let [ext (loco/extension-of file-path)
        fn-adapter (function-body-parsing/function-adapter-for ext input special-cases-requiring-line-separators)]
    (if (= ext ".py") ; WARNING: special case
      (try
        (parse-nested-python-complexity-for file-path input functions)
        (catch Throwable e
          (do
            (timbre/error "X-Ray Python nesting complexity failed to parse the file " file-path e)
            (throw (ParseException. (str "X-Ray complexity calculator failed to parse " file-path) 0)))))
      (let [parser (get available-parsers ext)]
        (if parser
          (try
            (performance/with-timbre-exe-time-info
              (map (partial cyclomatic-complexity-for
                            (partial parser file-path))
                   (fn-adapter functions)))
            (catch Throwable e
              (do
                (timbre/error "X-Ray complexity failed to parse the file " file-path e)
                (throw (ParseException. (str "X-Ray complexity calculator failed to parse " file-path) 0)))))
          (map (fn [s] (assoc s :cc unknown-complexity-result)) functions))))))

(defn max-nested-complexity-in-file
  "Some languages (e.g. PHP, Python) lets you write code in a global scope outside of functions.
   We want to detect islands of complexity there as well."
  [file-path input]
  (let [ext (loco/extension-of file-path)
        parser (get nested-complexity-parsers ext)]
    (if (and (some? parser)
             (languages-with-complexity-in-global-scope ext))
        (try
          (performance/with-timbre-exe-time-info
            (parser file-path input))
          (catch Throwable e
            (do
              (timbre/error "X-Ray global nested complexity failed to parse the file " file-path e)
              (throw (ParseException. (str "X-Ray global nested complexity calculator failed to parse " file-path) 0)))))
        low-default-complexity)))

(defn by-deepest-nested-in-file
  "Calculates the deepest nested complexity per function in the given file."
  [file-path input functions]
  (->> functions
       (custom-complexity-for-functions-in-file file-path input nested-complexity-parsers)
       (map (fn [{:keys [cc] :as v}] (assoc v :nested-complexity cc)))
       (map (fn [v] (dissoc v :cc)))))