(ns hotspots-x-ray.recommendations.code-comments.pattern-statistics
  (:require [clojure.spec.alpha :as s]
            [evolutionary-metrics.complexity.loco :as loco]
            [evolutionary-metrics.trends.complexity-trend :as ctrends]
            [taoensso.timbre :as log]))

(s/def ::n-matches nat-int?)
(s/def ::pattern-name string?)

(s/def ::code-comments-match (s/keys :req-un [::n-matches
                                              ::pattern-name]))

(s/def ::code-comments-matches (s/keys :opt-un [::code-comment-match]))

(s/def ::biomarker-comment-regex string?)
(s/def ::biomarker-comment-match-name ::pattern-name)

(s/def ::biomarker-comment-config (s/keys :opt-req [::biomarker-comment-regex
                                                    ::biomarker-comment-regex-name]))

(s/def ::biomarker-comment-configs (s/coll-of ::biomarker-comment-config))

(s/def ::minimum-project-spec (s/keys :opt-un [::biomarker-comment-configs]))

(defn- initial-match-state-from
  [biomarker-comment-configs {:keys [commented-line?] :or {commented-line? (constantly false)} :as _language-rules}]
  (mapv (fn [{:keys [biomarker-comment-regex] :as m}]
          {:expr-to-match (assoc m :biomarker-comment-regex (re-pattern biomarker-comment-regex))
           :commented-line? commented-line?
           :n-matches 0})
        biomarker-comment-configs))

(defn- inc-when-match-in
  [line match-state]
  (mapv (fn [{:keys [expr-to-match n-matches] :as m}]
          (if (re-find (:biomarker-comment-regex expr-to-match) line)
            (assoc m :n-matches (inc n-matches))
            m))
        match-state))

(defn- inc-when-match-in-commented-code-line
  [line match-state]
  (mapv (fn [{:keys [expr-to-match n-matches commented-line?] :as m}]
          (if (and (commented-line? line)
                   (re-find (:biomarker-comment-regex expr-to-match) line))
            (assoc m :n-matches (inc n-matches))
            m))
        match-state))

(deftype CommentPatternMatchAccumulator [match-state in-multi-state]
  loco/CodeStatsAccumulator
  (blanks [this] (CommentPatternMatchAccumulator. match-state in-multi-state))
  (comments [this line] (CommentPatternMatchAccumulator. (inc-when-match-in line match-state) in-multi-state))
  (codes [this line] (CommentPatternMatchAccumulator. (inc-when-match-in-commented-code-line line match-state) in-multi-state))
  (in-multi? [this] in-multi-state)
  (start-multi-comment [this line] (CommentPatternMatchAccumulator. (inc-when-match-in line match-state) true))
  (end-multi-comment [this line] (CommentPatternMatchAccumulator. (inc-when-match-in line match-state) false))
  (stats [this]
    (map (fn [{:keys [expr-to-match n-matches]}]
           {:n-matches n-matches
            :pattern-name (:biomarker-comment-regex-name expr-to-match)})
         match-state)))

(defn- count-matches-in
  [file-path
   input
   {:keys [biomarker-comment-configs] :as _project}]
  (let [language-rules (ctrends/language-rules-for file-path)
        lines (clojure.string/split-lines input)] ; Loco operates on seq of lines, which allows for streaming if needed
    (->> (CommentPatternMatchAccumulator. (initial-match-state-from biomarker-comment-configs language-rules) false)
         (loco/reduce-stats-in lines language-rules)
         .stats)))

(defn- comment-pattern-specified-in?
  [{:keys [biomarker-comment-configs] :as _project}]
  (->> biomarker-comment-configs
       (filter (fn [{:keys [biomarker-comment-regex biomarker-comment-regex-name]}]
                 (not (or (empty? biomarker-comment-regex)
                          (empty? biomarker-comment-regex-name)))))
       empty?
       not))

(defn comments-matching
  [project file-path input]
  (if (comment-pattern-specified-in? project)
    (do
      (log/debug "Code comment biomarkers enabled: scanning " (count (get project :biomarker-comment-configs [])) " combinations.")
      {:code-comment-match (doall (count-matches-in file-path input project))})
    {}))

(s/fdef comments-matching
        :args (s/cat :project ::minimum-project-spec
                     :file-path string?
                     :input string?)
        :ret ::code-comments-matches)

