(ns hotspots-x-ray.detectors.change-detector
  (:require [taoensso.timbre :as timbre]
            [hotspots-x-ray.languages.parser :as parser]
            [hotspots-x-ray.cleansing.functions :as cleanse]
            [hotspots-x-ray.recommendations.similarity :as similarity]
            [evolutionary-metrics.trends.complexity-trend :as maat-trends]
            [evolutionary-metrics.mining.ws-complexity :as maat-wsc]
            [clojure.set :as s]
            [clojure.math.combinatorics :as combo])
  (:import (java.text ParseException)
           (java.io StringReader)))

(defn- describe-fn-evolution
  [origin-fns new-fns]
  (let [origin-names (->> origin-fns keys (into #{}))
        new-names (->> new-fns keys (into #{}))]
    {:same-names (s/intersection origin-names new-names)
     :new-names (s/difference new-names origin-names)
     :removed-names (s/difference origin-names new-names)}))

(defn- complexity-snapshot-of
  [language-dependent-rules date content]
  (let [{:keys [total n mean median sd max comments complexity-loc-ratio]}
        (maat-wsc/total-indent-complexity maat-trends/options
                                          content
                                          language-dependent-rules)]
      [date total n mean median sd max comments complexity-loc-ratio]))

(defn- with-complexity-metrics
  [language-dependent-rules new-content date new-state changes]
  (let [change? (set changes)
        changed-fn-metrics (filter (comp change? :name) new-state)
        content-by-fn (cleanse/content-by-function-names new-content changed-fn-metrics)]
    (map (fn [[change change-content]] {:name change
                                        :complexity (complexity-snapshot-of language-dependent-rules date change-content)})
         content-by-fn)))

(defn- comparable-fn-of
  [function-name fns]
  {:name function-name :body (get fns function-name "")})

;; We need to be somewhat liberal when trying identify renamed functions.
;; We cannot check for 100 similarity on the function bodies because this
;; will fail if another function is renamed too and that function is
;; invoked from the body of this one. So try a lower threshold here - it's
;; all a heuristic.
(def ^:private rename-similarity-threshold 95)

(defn- renamed-function?
  [origin-fns new-fns [old-name new-name]]
  (let [f1 (comparable-fn-of old-name origin-fns)
        f2 (comparable-fn-of new-name new-fns)
        s (similarity/similarity-in-percent f1 f2 {})]
    (>= (:similarity s) rename-similarity-threshold)))

(defn- name-tracking-context
  [[old-name new-name]]
  {:old-name old-name
   :new-name new-name})

(defn detect-renamed-functions
  [new-names removed-names origin-fns new-fns]
  (let [candidates (combo/cartesian-product removed-names new-names)]
    (if (some? candidates) ; I hate that we get nil back in case one seq is empty
      (->> candidates
           (filter (partial renamed-function? origin-fns new-fns))
           (map name-tracking-context))
      [])))

(defn- function-body-changed?
  "Encapsulate the rule to detect change."
  [f1 f2]
  (not= f1 f2))

(defn- changed?
  [origin-fns new-fns function-name]
  (let [f1 (get origin-fns function-name "")
        f2 (get new-fns function-name " ")]
    (function-body-changed? f1 f2)))

(defn- renamed-changed?
  "Our algorithm doesn't detect changes in renamed functions because
   the functions (obviously) have different names in the two revisions.
   That's why we need to run this extra step that takes renames into account."
  [origin-fns new-fns {:keys [old-name new-name]}]
  (let [f1 (get origin-fns old-name "")
        f2 (get new-fns new-name " ")]
    (function-body-changed? f1 f2)))

(defn- changed-functions-by-similarity
  [language-dependent-rules new-content date origin-state new-state]
  (let [origin-fns (cleanse/current-function-bodies-in origin-state)
        new-fns (cleanse/current-function-bodies-in new-state)
        {:keys [same-names new-names removed-names]} (describe-fn-evolution origin-fns new-fns)
        changed (filter (partial changed? origin-fns new-fns) same-names)
        renamed (detect-renamed-functions new-names removed-names origin-fns new-fns)
        renamed-and-changed (filter (partial renamed-changed? origin-fns new-fns) renamed)
        renamed-changes (map :new-name renamed-and-changed)
        all-changes-in-revision (into changed renamed-changes)
        all-changes-with-complexity-for-trends (with-complexity-metrics language-dependent-rules new-content date new-state all-changes-in-revision)]
    {:changed all-changes-with-complexity-for-trends
     :renamed renamed}))

(defn- without-code
  [r]
  (dissoc r :content))

(defn- functions-in
  [file-name rev content]
  (timbre/trace "Start parsing revision " rev)
  (let [r (parser/parse-function-statistics file-name (StringReader. content))]
    (timbre/trace "Successfully parsed revision " rev)
    r))

(defn changes-between
  [file-name [oldest newest]]
  (try
    (timbre/trace "X-Ray investigating changes between: " (:rev oldest) (:rev newest))
    (let [date-of-change (:date newest)
          new-content (:content newest)
          language-dependent-complexity-rules (maat-trends/language-rules-for file-name)
          origin-state (functions-in file-name (:rev oldest) (:content oldest))
          new-state (functions-in file-name (:rev newest) new-content)
          {:keys [changed renamed]} (changed-functions-by-similarity language-dependent-complexity-rules
                                                                     new-content
                                                                     date-of-change
                                                                     origin-state
                                                                     new-state)]
      (timbre/trace "X-Ray between revision " (without-code oldest) " and " (without-code newest)
                    " found the following changed functions: " (pr-str changed)
                    " and the following renamed functions: " (pr-str renamed))
      {:changes changed :renames renamed :rev (:rev newest) :date date-of-change})
    (catch ParseException e
      (do
        (timbre/error "X-Ray parse failure - ignoring evolution between " (without-code oldest) " and " (without-code newest) e)
        {:changes [] :renames [] :rev (:rev newest) :date (:date newest)}))))
