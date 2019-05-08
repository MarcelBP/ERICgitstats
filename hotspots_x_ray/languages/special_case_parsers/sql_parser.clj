(ns hotspots-x-ray.languages.special-case-parsers.sql-parser
  "SQL parser which tries to detect SQL dialect and use a proper parser.
  By default, it uses PL/SQL parser."
  (:require [hotspots-x-ray.languages.antlr :as antlr]
            [taoensso.timbre :as log])
  (:import (hotspots_x_ray.languages PLSqlFunctionListener TSqlFunctionListener)
           (hotspots_x_ray.languages.generated PLSqlLexer PLSqlParser
                                               TSqlLexer TSqlParser)))

;; We'd like rely on the presence of GO command (for submitting batches) in T-SQL.
;; There's no such thing in PL/SQL and it's very likely that files containing functions/procedures
;; contains GO commands
;; HOWEVER: we can't do that because otherwise we fail to detect T-SQL in simple files
;; in which there is only a single procedure/function without any GO command!
(def tsql-regex #"(?im)^\s*go\s*$")

(def ^:private plsql-parser
  (antlr/make-parser {:lexer-ctor    #(PLSqlLexer. %1)
                      :parser-ctor   #(PLSqlParser. %1)
                      :context-fn    #(.translationunit %1)
                      :listener-ctor #(PLSqlFunctionListener.)}))

(def ^:private tsql-parser
  (antlr/make-parser {:lexer-ctor    #(TSqlLexer. %1)
                      :parser-ctor   #(TSqlParser. %1)
                      :context-fn    #(.translationunit %1)
                      :listener-ctor #(TSqlFunctionListener.)}))

(defn- match-tsql [file-path input]
  ;; don't use regex because it fails to detect simple files with missing GO statement
  ;; although regex is way faster, we need to use real parser!
  #_(re-seq tsql-regex input)
  (seq (tsql-parser antlr/->parsed-functions file-path input)))

(defn parse-sql
  "Parses SQL dialect by choosing proper parser.
  Note that we expect reader as an input unlike other parsers which usually also accept plain string."
  [result-maker file-path input-reader]
  (let [input (slurp input-reader)]
    (if (match-tsql file-path input)
      (do
        (log/debug "Parsing input with T-SQL parser.")
        (tsql-parser result-maker file-path input))
      (do
        (log/debug "Parsing input with default PLSQL parser.")
        (plsql-parser result-maker file-path input)))))

(comment

  (parse-sql "some.sql" (clojure.java.io/reader "my-file.sql"))
  
  )
