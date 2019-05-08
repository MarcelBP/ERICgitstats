(ns hotspots-x-ray.languages.special-case-parsers.rsa-rte-model-parser
  (:require [clojure.data.xml :as xml]
            [xml-in.core :as xml-in]
            [evolutionary-metrics.complexity.loco :as loco]))

;; This module implements a parser for Rational Rose models. The parsed models are adapted to the
;; common functions format that we use internally.
;; This module is also a special case since we don't need to provide a specific parser; Rose models are
;; XML so let's use generic XML libraries to deal with the "code".
;;
;; Here's the overall algorithm:
;;
;; 1. The models are XML (or "XMI" in model-driven snake oil speech).
;; 2. From there we can extract `ownedBehavior` tags, which contain a `<language>C++</language>` child.
;; The  `ownedBehavior` also contains a `name` attribute which would be equal to the function name.
;; 3. Using the data generated in #2, we can transform the data into our internal X-Ray format and re-use all
;; the X-Ray algorithms and the C++ cyclomatic complexity calculator.

(defn- class-in
  [x]
  (xml-in/find-in x [(xml-in/tag= :xmi:XMI)
                     (xml-in/tag= :uml:Class)]))

(defn- packaged-elements-in
  [x]
  (xml-in/find-in x [(xml-in/tag= :xmi:XMI)
                     (xml-in/tag= :uml:Package)
                     (xml-in/tag= :packagedElement)]))

(defn- owned-behaviors-in
  [xs]
  (filter (comp (partial = :ownedBehavior) :tag) xs))

(defn- owned-attributes-in
  [xs]
  (filter (comp (partial = :ownedAttribute) :tag) xs))

(defn- owned-operations-in
  [xs]
  (filter (comp (partial = :ownedOperation) :tag) xs))

(defn- return-spec?
  [{:keys [attrs]}]
  (= "return" (:direction attrs)))

(defn- function-argument-types-in
  [ops]
  (->> ops
       :content
       (map :attrs)
       (map :type)))

(defn- argument-type-of
  [b]
  (let [xmi-type (:type (:attrs b))]
    (if (= "uml:PrimitiveType" xmi-type)
      "primitive"
      (if (some? xmi-type) xmi-type "unknown"))))

(defn- argument->type-description
  [b]
  (let [content (:content b)]
    (if (some? content)
      (argument-type-of content)
      "unknown")))

(defn- function-arguments-in
  [{:keys [attrs content] :as _b}]
  (let [params (->> content
                    (filter (comp (partial = :ownedParameter) :tag))
                    (remove return-spec?)
                    (map argument->type-description))
        fn-name (:name attrs)]
    {:name fn-name
     :calculated true ; make it possible to filter away unsupported languages
     :n-args (count params)
     :n-primitives (->> params (filter (partial = "primitive")) count)
     :n-string-args 0 ; No support...yet?
     :types params}))

(defn- cpp-code?
  [b]
  (-> b
      (xml-in/find-in [(xml-in/tag= :ownedBehavior)
                       (xml-in/tag= :language)])
      first
      (= "C++")))

(defn- cpp-code-in
  [bs]
  (filter cpp-code? bs))

(def ^:private location-meta (comp :clojure.data.xml/location-info meta))

(defn function-in
  [{:keys [attrs] :as b}]
  (let [code-body  (xml-in/find-in b [(xml-in/tag= :ownedBehavior)
                                      (xml-in/tag= :body)])
        raw-code (first code-body)
        start-line (+ (:line-number (location-meta b)) 2) ; offset for the initial XMI elements
        fn-length (-> raw-code clojure.string/split-lines count)
        end-line (+ start-line fn-length -1)]
    {:name (:name attrs)
     :body raw-code
     :start-line start-line
     :end-line end-line}))

(defn- named-code-content-from
  [bs]
  (map function-in bs))

(defn- has-attribute-name?
  [oa]
  (and (some? (:attrs oa))
       (some? (:name (:attrs oa)))))

(defn- attribute-name-of
  [{:keys [attrs] :as _a}]
  (:name attrs))

(defn- named-attributes-in
  [osb]
  (->> osb
       owned-attributes-in
       (filter has-attribute-name?)
       (map attribute-name-of)))

(defn- all-functions-in
  [osb]
  (-> osb
      owned-behaviors-in
      cpp-code-in
      named-code-content-from))

(defn- parse-packaged-model
  [parsed-xml]
  (-> parsed-xml
      packaged-elements-in
      all-functions-in))

(defn- args-by-function
  [oo]
  (map function-arguments-in oo))

(defn- parse-function-arguments-in-packaged-model
  [parsed-xml]
  (-> parsed-xml
      packaged-elements-in
      owned-operations-in
      args-by-function))

(defn- parse-function-arguments-in-fragment
  [parsed-xml]
  (-> parsed-xml
      class-in
      owned-operations-in
      args-by-function))

(defn- parse-fragments
  [parsed-xml]
  (-> parsed-xml
      class-in
      all-functions-in))

(defn- parse-attributes-in-packaged-model
  [parsed-xml]
  (-> parsed-xml
      packaged-elements-in
      owned-attributes-in))

(defn- parse-attributes-in-fragments
  [parsed-xml]
  (-> parsed-xml
      class-in
      named-attributes-in))

(def ^:private attributes-parsers
  {".emx" parse-attributes-in-packaged-model
   ".efx" parse-attributes-in-fragments})

(def ^:private function-parsers
  {".emx" parse-packaged-model
   ".efx" parse-fragments})

(def ^:private function-arguments-parsers
  {".emx" parse-function-arguments-in-packaged-model
   ".efx" parse-function-arguments-in-fragment})

(defn- parse-using
  "General parsing algorithm se-used for both function and function arguments parsing."
  [parsers file-path input]
  (let [adapted-input (if (instance? java.lang.String input) (java.io.StringReader. input) input) ; to work for both X-Ray and biomarkers
        xp (xml/parse adapted-input :namespace-aware false)
        ext (loco/extension-of file-path)
        parser (get parsers ext)]
    (parser xp)))

(defn parse-rsa-rte-model
  [_conversions-for-antlr-based-parsers file-path input]
  (parse-using function-parsers file-path input))

(defn parse-rsa-rte-function-arguments
  [file-path input]
  (parse-using function-arguments-parsers file-path input))

(defn parse-cohesion-properties
  [result-maker file-path input]
  (let [fns (parse-rsa-rte-model result-maker file-path input)
        fields (parse-using attributes-parsers file-path input)]
    {:functions fns
     :fields fields}))