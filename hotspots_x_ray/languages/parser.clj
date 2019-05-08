(ns hotspots-x-ray.languages.parser
  (:require [clojure.spec.alpha :as s]
            [evolutionary-metrics.complexity.loco :as loco]
            [hotspots-x-ray.diagnostics.performance :as performance]
            [hotspots-x-ray.languages.antlr :as antlr]
            [hotspots-x-ray.languages.specs :as specs]
            [hotspots-x-ray.languages.special-case-parsers.rsa-rte-model-parser :as rsa-rte-parser]
            [hotspots-x-ray.languages.special-case-parsers.sql-parser :as sql]
            [taoensso.timbre :as timbre])
  (:import [org.antlr.v4.runtime CommonTokenStream ANTLRInputStream]
           [org.antlr.v4.runtime.tree ParseTreeWalker]
           (hotspots_x_ray.languages.generated ClojureLexer
                                               ClojureParser CSharpMicroLexer CSharpMicroParser
                                               CppMicroParser CppMicroLexer
                                               RubyMicroLexer RubyMicroParser
                                               Python3Lexer Python3Parser
                                               ErlangLexer ErlangParser
                                               ScalaLexer ScalaParser
                                               ECMAScriptLexer ECMAScriptParser
                                               ObjectiveCMicroLexer ObjectiveCMicroParser
                                               GoLexer GoParser
                                               PHPLexer PHPParser
                                               VisualBasicLexer VisualBasicParser
                                               ApexLexer ApexParser
                                               JavaMicroLexer JavaMicroParser
                                               GroovyLexer GroovyParser
                                               SwiftLexer SwiftParser
                                               KotlinLexer KotlinParser PerlLexer PerlParser)
           (hotspots_x_ray.languages ClojureDefinitionListener
                                     CSharpMethodMicroListener CppMicroFunctionListener
                                     RubyMicroFunctionListener
                                     Python3FunctionListener
                                     ErlangFunctionListener
                                     ScalaMethodListener
                                     EcmaScriptFunctionListener
                                     ObjectiveCMicroMethodListener
                                     GoFunctionListener
                                     PhpFunctionListener
                                     VisualBasicFunctionListener
                                     ApexMethodListener
                                     JavaMethodListener
                                     GroovyMethodListener
                                     SwiftFunctionListener
                                     KotlinFunctionListener PerlFunctionListener)
           (java.text ParseException)))

(def ^:private cpp-parser
  (antlr/make-parser {:lexer-ctor    #(CppMicroLexer. %1)
                      :parser-ctor   #(CppMicroParser. %1)
                      :context-fn    #(.translationunit %1)
                      :listener-ctor #(CppMicroFunctionListener.)}))

(def ^:private objective-c-parser
  (antlr/make-parser {:lexer-ctor    #(ObjectiveCMicroLexer. %1)
                      :parser-ctor   #(ObjectiveCMicroParser. %1)
                      :context-fn    #(.translationunit %1)
                      :listener-ctor #(ObjectiveCMicroMethodListener.)}))

(def ^:private extended-ecma-script-parser
  (antlr/make-parser {:lexer-ctor    #(ECMAScriptLexer. %1)
                      :parser-ctor   #(ECMAScriptParser. %1)
                      :context-fn    #(.translationunit %1)
                      :listener-ctor #(EcmaScriptFunctionListener.)}))

(def ^:private apex-parser
  (antlr/make-parser {:lexer-ctor    #(ApexLexer. %1)
                      :parser-ctor   #(ApexParser. %1)
                      :context-fn    #(.translationunit %1)
                      :listener-ctor #(ApexMethodListener.)}))

(def ^:private clojure-parser
  (antlr/make-parser {:lexer-ctor    #(ClojureLexer. %1)
                      :parser-ctor   #(ClojureParser. %1)
                      :context-fn    #(.file %)
                      :listener-ctor #(ClojureDefinitionListener.)}))

(def ^:private perl-parser
  (antlr/make-parser {:lexer-ctor    #(PerlLexer. %1)
                      :parser-ctor   #(PerlParser. %1)
                      :context-fn    #(.translationunit %)
                      :listener-ctor #(PerlFunctionListener.)}))

(def ^:private parsers
  {".c"    cpp-parser ; the C++ parser can parse C code too since it's a subset and ...
   ".h"    cpp-parser ; ..the C++ parser is more performant and liberal than a real C parser.
   ".cc"   cpp-parser
   ".cpp"  cpp-parser
   ".cxx"  cpp-parser
   ".hh"   cpp-parser
   ".hpp"  cpp-parser
   ".hxx"  cpp-parser
   ".m"    objective-c-parser
   ".mm"   objective-c-parser
   ".clj"  clojure-parser
   ".cljs" clojure-parser
   ".cljc" clojure-parser

   ; files for Rational Software Architect models
   ".efx"   rsa-rte-parser/parse-rsa-rte-model
   ".emx"   rsa-rte-parser/parse-rsa-rte-model

   ".cs"   (antlr/make-parser {:lexer-ctor    #(CSharpMicroLexer. %1)
                               :parser-ctor   #(CSharpMicroParser. %1)
                               :context-fn    #(.translationunit %1)
                               :listener-ctor #(CSharpMethodMicroListener.)})
   ".go"   (antlr/make-parser {:lexer-ctor    #(GoLexer. %1)
                               :parser-ctor   #(GoParser. %1)
                               :context-fn    #(.translationunit %1)
                               :listener-ctor #(GoFunctionListener.)})
   ".groovy" (antlr/make-parser {:lexer-ctor    #(GroovyLexer. %1)
                                 :parser-ctor   #(GroovyParser. %1)
                                 :context-fn    #(.translationunit %1)
                                 :listener-ctor #(GroovyMethodListener.)})
   ".rb"   (antlr/make-parser {:lexer-ctor      #(RubyMicroLexer. %1)
                               :parser-ctor     #(RubyMicroParser. %1)
                               :context-fn      #(.prog %1)
                               :listener-ctor   #(RubyMicroFunctionListener.)
                               :remove-overlaps false})
   ".py"   (antlr/make-parser {:lexer-ctor    #(Python3Lexer. %1)
                               :parser-ctor   #(Python3Parser. %1)
                               :context-fn    #(.file_input %1)
                               :listener-ctor #(Python3FunctionListener.)})
   ".erl"  (antlr/make-parser {:lexer-ctor    #(ErlangLexer. %1)
                               :parser-ctor   #(ErlangParser. %1)
                               :context-fn    #(.forms %1)
                               :listener-ctor #(ErlangFunctionListener.)})
   ".scala" (antlr/make-parser {:lexer-ctor    #(ScalaLexer. %1)
                                :parser-ctor   #(ScalaParser. %1)
                                :context-fn    #(.translationunit %1)
                                :listener-ctor #(ScalaMethodListener.)})
   ".php" (antlr/make-parser {:lexer-ctor    #(PHPLexer. %1)
                              :parser-ctor   #(PHPParser. %1)
                              :context-fn    #(.translationunit %1)
                              :listener-ctor #(PhpFunctionListener.)})
   ".vb" (antlr/make-parser {:lexer-ctor    #(VisualBasicLexer. %1)
                             :parser-ctor   #(VisualBasicParser. %1)
                             :context-fn    #(.translationunit %1)
                             :listener-ctor #(VisualBasicFunctionListener.)})

   ".js"    extended-ecma-script-parser
   ".ts"    extended-ecma-script-parser
   ".jsx"   extended-ecma-script-parser
   ".tsx"   extended-ecma-script-parser
   ".vue"   extended-ecma-script-parser

   ".java" (antlr/make-parser {:lexer-ctor    #(JavaMicroLexer. %1)
                               :parser-ctor   #(JavaMicroParser. %1)
                               :context-fn    #(.translationunit %1)
                               :listener-ctor #(JavaMethodListener.)})
   ".cls"      apex-parser
   ".tgr"      apex-parser
   ".trigger"  apex-parser
   ".swift" (antlr/make-parser {:lexer-ctor    #(SwiftLexer. %1)
                                :parser-ctor   #(SwiftParser. %1)
                                :context-fn    #(.translationunit %1)
                                :listener-ctor #(SwiftFunctionListener.)})
   ".kt" (antlr/make-parser {:lexer-ctor    #(KotlinLexer. %1)
                             :parser-ctor   #(KotlinParser. %1)
                             :context-fn    #(.translationunit %1)
                             :listener-ctor #(KotlinFunctionListener.)})
   ".sql"   sql/parse-sql
   ".psql"  sql/parse-sql
   ".plsql" sql/parse-sql
   ".tsql"  sql/parse-sql
   ".prc"   sql/parse-sql
   ".fnc"   sql/parse-sql
   ".bdy"   sql/parse-sql

   ".pl"    perl-parser
   ".pm"    perl-parser})

(defn supported-parsers
  []
  (keys parsers))

(defn supports-file-type?
  [file-path]
  (->> (loco/extension-of file-path)
       (get parsers)
       some?))

(defn- handle-parser-error
  [file-path e]
  (do
    (timbre/error e)
    (timbre/error "X-Ray failed to parse the file " file-path e)
    (throw (ParseException. (str "X-Ray failed to parse " file-path) 0))))

(defn parse-function-statistics
  [file-path input]
  {:post [(s/valid? ::specs/parsed-function-statistics %)]}
  (let [ext (loco/extension-of file-path)]
    (if-let [parser (get parsers ext)]
      (try
        (performance/with-timbre-exe-time-info
          (parser antlr/->parsed-functions file-path input))
        ;; Don't catch Throwable! See https://trello.com/c/VfZDXacU/737-outofmemoryerror-during-x-ray-analysis-tencentcom
        (catch Exception e
          (handle-parser-error file-path e))
        ;; This is controversial, but perhaps may be thrown by ANTLR
        (catch AssertionError e
          (handle-parser-error file-path e)))
      (throw (Exception. (str "No parser available for file extension " ext
                              ". Supported extensions are: " supported-parsers))))))
