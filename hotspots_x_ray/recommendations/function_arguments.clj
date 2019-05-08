(ns hotspots-x-ray.recommendations.function-arguments
  (:require [taoensso.timbre :as timbre]
            [hotspots-x-ray.diagnostics.parse-errors :as parse-errors]
            [hotspots-x-ray.diagnostics.performance :as performance]
            [evolutionary-metrics.complexity.loco :as loco]
            [hotspots-x-ray.languages.antlr :as antlr]
            [codescene.stats.math :as m]
            [hotspots-x-ray.languages.special-case-parsers.rsa-rte-model-parser :as rsa-rte-parser])
  (:import (hotspots_x_ray.diagnostics PersistentParseErrorListener)
           (org.antlr.v4.runtime.tree ParseTreeWalker)
           (org.antlr.v4.runtime CommonTokenStream ANTLRInputStream)
           (hotspots_x_ray.languages.arguments JavaMethodArgumentsExtractor
                                               CSharpMethodArgumentsExtractor
                                               GroovyMethodArgumentsExtractor
                                               CppMethodArgumentsExtractor
                                               ECMAScriptMethodArgumentsExtractor
                                               ScalaMethodArgumentsExtractor
                                               ObjectiveCMethodArgumentsExtractor
                                               PerlMethodArgumentsExtractor
                                               PythonMethodArgumentsExtractor
                                               SwiftMethodArgumentsExtractor
                                               VisualBasicMethodArgumentsExtractor
                                               GoMethodArgumentsExtractor
                                               PhpMethodArgumentsExtractor
                                               RubyMethodArgumentsExtractor
                                               KotlinMethodArgumentsExtractor
                                               ErlangMethodArgumentsExtractor)
           (hotspots_x_ray.languages.generated JavaMethodArgumentsParser JavaMethodArgumentsLexer
                                               CSharpMethodArgumentsParser CSharpMethodArgumentsLexer
                                               GroovyMethodArgumentsParser GroovyMethodArgumentsLexer
                                               CppMethodArgumentsParser CppMethodArgumentsLexer
                                               ECMAScriptLexer ECMAScriptParser
                                               ScalaMethodArgumentsParser ScalaMethodArgumentsLexer
                                               ObjectiveCMethodArgumentsParser ObjectiveCMethodArgumentsLexer
                                               PerlLexer PerlParser
                                               Python3Lexer Python3Parser
                                               SwiftMethodArgumentsParser SwiftMethodArgumentsLexer
                                               VisualBasicLexer VisualBasicParser
                                               GoMethodArgumentsParser GoMethodArgumentsLexer
                                               PHPLexer PHPParser
                                               RubyMicroLexer RubyMicroParser
                                               KotlinLexer KotlinParser
                                               ErlangLexer ErlangParser)
           (java.text ParseException)))

(defn- sum-for
  [pred vs]
  (->> vs (filter pred) count))

(defn- statistics-for
  [type-names {:keys [primitives string-types]}]
  {:calculated true ; make it possible to filter away unsupported languages
   :n-args (count type-names)
   :n-primitives (sum-for primitives type-names)
   :n-string-args (sum-for string-types type-names)
   :types type-names})

(defn- make-antlr-parser
  "Given a map of ANTLR constructors and functions, return a parser function."
  [{:keys [lexer-ctor parser-ctor context-fn listener-ctor primitive-types]}]
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
      (let [type-names (map (fn [a] (.typeName a)) (.arguments listener))
            result (statistics-for type-names primitive-types)]
        (parse-errors/trace-potential-errors file-path error-listener)
        (.removeErrorListeners parser)
        result))))

(def ^:private Java-primitive-types
  {:primitives #{"byte" "short" "int" "long" "float" "double" "boolean" "char"
                 "Integer" "Long" "Float" "Double" "Boolean"
                 "String" "Object"}
   :string-types #{"String"}})

(def ^:private Groovy-primitive-types Java-primitive-types)

(def ^:private CSharp-primitive-types
  {:primitives #{"bool" "Boolean" "byte" "Byte" "sbyte" "SByte"
                 "char" "Char" "decimal" "Decimal" "double" "Double"
                 "float" "Single" "int" "Int32" "uint" "UInt32"
                 "long" "Int64" "ulong" "UInt64" "object" "Object"
                 "short" "Int16" "ushort" "UInt16" "Guid"
                 "String" "string"}
   :string-types #{"String" "string"}})

(def ^:private Cpp-primitive-types
  {:primitives #{"short" "int" "long" "float" "double" "bool" "char"
                 "unsignedshort" "unsignedint" "unsignedlong" "unsignedchar"
                 "string" "std::string"}
   :string-types #{"char" "unsignedchar" "string" "std::string" "CString"}})

(def ^:private TypeScript-primitive-types
  {:primitives #{"boolean" "string" "string[]" "number" "number[]"}
   :string-types #{"string" "string[]"}})

(def ^:private Scala-primitive-types
  {:primitives #{"Boolean" "Byte" "Short" "Int" "Long" "Float" "Double" "Char" "String"}
   :string-types #{"String"}})

(def ^:private ObjectiveC-primitive-types
  {:primitives #{"char" "int" "float" "double" "short" "long" "long long" "BOOL"
                 "bool"
                 "unsignedshort" "unsignedint" "unsignedlong" "unsignedchar"
                 "NSString" "NSUInteger" "NSDecimal" "NSDecimalNumber"
                 "NSNumber"
                 "string" "std::string"}
   :string-types #{"char" "unsignedchar" "NSString" "string" "std::string"}})

(def ^:private Swift-primitive-types
  {:primitives #{"Int" "UInt" "Int8" "Int16" "Int32" "Int64"
                 "UInt8" "UInt16" "UInt32" "UInt64"
                 "Float" "Double" "Bool" "Character" "String" "NSString"}
   :string-types #{"String" "NSString"}})

; NOTE: Since VB is case insensitive, the parser converts to lowercase so that's what we specify:
(def ^:private VisualBasic-primitive-types
  {:primitives #{"boolean" "byte" "char" "date" "datetime" "decimal" "double"
                 "integer" "int32" "Long" "int64" "object" "sbyte"
                 "short" "int16" "single" "string"
                 "uinteger" "uint32" "ulong" "uint64" "ushort" "uint16"
                 "guid"}
   :string-types #{"string"}})

(def ^:private Go-primitive-types
  {:primitives #{"bool" "string"
                 "int" "int8" "int16" "int32" "int64"
                 "uint" "uint8" "uint16" "uint32" "uint64" "uintptr"
                 "byte" "rune"
                 "float32" "float64"
                 "complex64" "complex128"}
   :string-types #{"string"}})

(def ^:private PHP-primitive-types
  {:primitives #{"int" "float"
                 "bool" "string"}
   :string-types #{"string"}})

(def ^:private kotlin-primitive-types
  {:primitives #{"Double" "Float"
                 "Long" "Int"
                 "Short" "Byte"
                 "Char"
                 "Boolean"
                 "UByte" "UShort" "UInt" "ULong"
                 "UByteArray" "UShortArray" "UIntArray" "ULongArray"
                 "String"}
   :string-types #{"String"}})

(def ^:private empty-set-for-dynamic-languages
  {:primitives #{}
   :string-types #{}})

(defmacro parser-for
  [base-name]
  `(make-antlr-parser {:lexer-ctor    #(~(symbol (str base-name "MethodArgumentsLexer.")) %1)
                       :parser-ctor   #(~(symbol (str base-name "MethodArgumentsParser.")) %1)
                       :context-fn    #(.singlefunctionscope %)
                       :listener-ctor #(~(symbol (str base-name "MethodArgumentsExtractor.")))
                       :primitive-types ~(symbol (str base-name "-primitive-types"))}))

(def ^:private cpp-parser (parser-for "Cpp"))

(def ^:private ecma-script-parser
  (make-antlr-parser {:lexer-ctor    #(ECMAScriptLexer. %1)
                      :parser-ctor   #(ECMAScriptParser. %1)
                      :context-fn    #(.function_level_expressions %1)
                      :listener-ctor #(ECMAScriptMethodArgumentsExtractor.)
                      :primitive-types TypeScript-primitive-types}))

(def ^:private perl-parser
  (make-antlr-parser {:lexer-ctor    #(PerlLexer. %1)
                      :parser-ctor   #(PerlParser. %1)
                      :context-fn    #(.translationunit %1)
                      :listener-ctor #(PerlMethodArgumentsExtractor.)
                      :primitive-types empty-set-for-dynamic-languages}))

(def ^:private erlang-parser
  (make-antlr-parser {:lexer-ctor    #(ErlangLexer. %1)
                      :parser-ctor   #(ErlangParser. %1)
                      :context-fn    #(.forms %1)
                      :listener-ctor #(ErlangMethodArgumentsExtractor.)
                      :primitive-types empty-set-for-dynamic-languages}))

(def ^:private python-parser
  (make-antlr-parser {:lexer-ctor    #(Python3Lexer. %1)
                      :parser-ctor   #(Python3Parser. %1)
                      :context-fn    #(.mc_cabe_complexity_entry_point %1)
                      :listener-ctor #(PythonMethodArgumentsExtractor.)
                      :primitive-types empty-set-for-dynamic-languages}))

(def ^:private vb-parser
  (make-antlr-parser {:lexer-ctor    #(VisualBasicLexer. %1)
                      :parser-ctor   #(VisualBasicParser. %1)
                      :context-fn     #(.translationunit %1)
                      :listener-ctor #(VisualBasicMethodArgumentsExtractor.)
                      :primitive-types VisualBasic-primitive-types}))

(def ^:private php-parser
  (make-antlr-parser {:lexer-ctor    #(PHPLexer. %1)
                      :parser-ctor   #(PHPParser. %1)
                      :context-fn     #(.translationunit %1)
                      :listener-ctor #(PhpMethodArgumentsExtractor.)
                      :primitive-types PHP-primitive-types}))

(def ^:private ruby-parser
  (make-antlr-parser {:lexer-ctor    #(RubyMicroLexer. %1)
                      :parser-ctor   #(RubyMicroParser. %1)
                      :context-fn     #(.prog %1)
                      :listener-ctor #(RubyMethodArgumentsExtractor.)
                      :primitive-types empty-set-for-dynamic-languages}))

(def ^:private kotlin-parser
  (make-antlr-parser {:lexer-ctor    #(KotlinLexer. %1)
                      :parser-ctor   #(KotlinParser. %1)
                      :context-fn     #(.translationunit %1)
                      :listener-ctor #(KotlinMethodArgumentsExtractor.)
                      :primitive-types kotlin-primitive-types}))

(def ^:private objective-c-parser (parser-for "ObjectiveC"))

(def ^:private arguments-parsers
  {".java"   (parser-for "Java")
   ".groovy" (parser-for "Groovy")

   ".cs"     (parser-for "CSharp")
   ".vb"     vb-parser

   ".erl"    erlang-parser

   ".go"     (parser-for "Go")

   ".kt"     kotlin-parser

   ".scala"  (parser-for "Scala")

   ".swift"  (parser-for "Swift")

   ".php"    php-parser

   ".rb"     ruby-parser

   ".c"       cpp-parser ; the C++ parser can parse C code too since it's a subset and ...
   ".h"       cpp-parser ; ..the C++ parser is more performant and liberal than a real C parser.
   ".cc"      cpp-parser
   ".cpp"     cpp-parser
   ".cxx"     cpp-parser
   ".hh"      cpp-parser
   ".hpp"     cpp-parser
   ".hxx"     cpp-parser

   ".pl"    perl-parser
   ".pm"    perl-parser

   ".m"    objective-c-parser
   ".mm"   objective-c-parser

   ".js"    ecma-script-parser
   ".ts"    ecma-script-parser
   ".jsx"   ecma-script-parser
   ".tsx"   ecma-script-parser
   ".vue"   ecma-script-parser

   ".py"    python-parser})

(def ^:private special-case-arguments-parsers
  ; files for Rational Software Architect models
  {".efx" rsa-rte-parser/parse-rsa-rte-function-arguments
   ".emx" rsa-rte-parser/parse-rsa-rte-function-arguments})

(defn- function-level-parsing-using
  [parsers file-path input]
  (let [ext (loco/extension-of file-path)]
    (if-let [parser (get parsers ext)]
      (try
        (performance/with-timbre-exe-time-info
          (parser file-path input))
        (catch Throwable e
          (do
            (timbre/error "X-Ray arguments failed to parse the file " file-path e)
            (throw (ParseException. (str "X-Ray arguments failed to parse " file-path) 0)))))
      {:calculated    false
       :n-args        0
       :n-string-args 0
       :n-primitives  0
       :types         []})))

(defn function-level
  [file-path {:keys [body constructor]}]
  (let [r (function-level-parsing-using arguments-parsers file-path body)]
    (if (and (some? constructor) constructor)
      (assoc r :constructor true)
      r)))

(defn- safe-max
 [vs]
 (if (empty? vs)
   0
   (apply max vs)))

(defn- summarize-function-argument-metrics
  [stats]
  (let [all-args (map :n-args stats)
        {:keys [mean]} (m/stats-for all-args)
        method-args (->> stats (remove :constructor))
        constructor-args (->> stats (filter :constructor))
        max-args (safe-max (map :n-args method-args))
        max-arg-name (->> method-args (filter (comp (partial = max-args) :n-args)) first :name)
        max-ctor-args (safe-max (map :n-args constructor-args))
        max-ctor-arg-name (->> constructor-args (filter (comp (partial = max-ctor-args) :n-args)) first :name)
        n-string-args (some->> stats (map :n-string-args) (reduce +))
        summary (reduce (fn [{:keys [n-args n-primitives]} r]
                          {:n-args       (+ n-args (:n-args r))
                           :n-primitives (+ n-primitives (:n-primitives r))})
                        {:n-args       0
                         :n-primitives 0}
                        stats)]
    {:fn-args (merge summary
                     {:mean-args     mean
                      :n-string-args n-string-args
                      :max-args      max-args
                      :max-arg-name  (or max-arg-name "<unknown>")
                      :max-ctor-args max-ctor-args
                      :max-ctor-arg-name  (or max-ctor-arg-name "<unknown>")})}))

(defn function-argument-metrics
  [file-path input raw-fns]
  (if (antlr/supports-file-type? file-path arguments-parsers)
    (let [stats (->> raw-fns
                     (map (fn [{:keys [name] :as fn-info}] (merge {:name name} (function-level file-path fn-info))))
                     (filter :calculated))]
      (summarize-function-argument-metrics stats))
    (if (antlr/supports-file-type? file-path special-case-arguments-parsers)
      (let [special-result-stats (function-level-parsing-using special-case-arguments-parsers file-path input)]
        (summarize-function-argument-metrics special-result-stats))
    {})))