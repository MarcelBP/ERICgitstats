(ns hotspots-x-ray.languages.antlr
  "ANTLR specific parser code.
  See `make-parser`.
  Note: there are multiple clones of `make-parser` function out there (called `make-antlr-parser`);
  it'd be nice to unify them."
  (:require [hotspots-x-ray.diagnostics.parse-errors :as parse-errors]
            [evolutionary-metrics.complexity.loco :as loco])
  (:import            (hotspots_x_ray.diagnostics PersistentParseErrorListener)
           (org.antlr.v4.runtime ANTLRInputStream
                                 CommonTokenStream)
           (org.antlr.v4.runtime.tree ParseTreeWalker)))

(defn supports-file-type?
  [file-path parsers]
  (->> (loco/extension-of file-path)
       (get parsers)
       some?))

(defn- to-internal-function-format
  "Maps a FunctionDefinition value to the map containing :name, :start-line and :end-line keys."
  [function-definition]
  (let [ctor (.-isConstructor function-definition)
        ctor-def (if ctor {:constructor true} {})] ; keep backwards compatibility due to all the tests...
    (merge
      {:name       (.-name function-definition)
       :start-line (.-startLine function-definition)
       :end-line   (.-endLine function-definition)
       :body       (.-body function-definition)}
      ctor-def)))

(defn parsed-fns->internal-format
  [fns]
  (map to-internal-function-format fns))

(defn ->parsed-functions
  [listener]
  (->> listener
       .getFunctions
       parsed-fns->internal-format
       (into [])))

(defn make-parser
  "Given a map of ANTLR constructors and functions, return a parser function.
   The parser function requires the following arguments:

    - result-maker: a function that accepts a parse listener. The return of
      the result-maker is the result of the (processed) parsing.

    - file-path: only used to deliver contextual error messages, if any.

    - input: a stream that we'll feed into the parser."
  [{:keys [lexer-ctor parser-ctor context-fn listener-ctor]}]
  (fn [result-maker file-path input]
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
      (let [results (result-maker listener)]
        (parse-errors/trace-potential-errors file-path error-listener)
        (.removeErrorListeners parser)
        results))))

