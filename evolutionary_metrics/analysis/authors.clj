;;; Copyright (C) 2013 Adam Tornhill, 2015 Empear

(ns evolutionary-metrics.analysis.authors
  (:require [clojure.spec.alpha :as s]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.parsers :as parsers]
            [clj-time.core :as tc]
            [evolutionary-metrics.analysis.internal.authors-shared :as authors-shared]))

;;; This module contains analysis methods related to the authors of the VCS commits.
;;; Research shows that these metrics (e.g. number of authors of a module) are
;;; related to the number of quality problems that module exhibits.

(s/def ::author ::core/author-name)
(s/def ::author-email ::core/author-email)

(s/def ::authors (s/keys :req-un [::author ::author-email]))
(s/def ::author-names (s/coll-of ::core/author-name))

(s/fdef all
        :args (s/cat :ds ::parsers/vcs-log)
        :ret ::author-names)
(defn all
  "Returns a set with the name of all authors."
  [commits]
  (distinct (map :author commits)))

(s/fdef authors-of-entity
        :args (s/cat :entity-group ::parsers/vcs-log)
        :ret nat-int?)
(defn- authors-of-entity
  [entity-group]
  (->>
    entity-group
    (map (juxt :author :author-email))
    distinct
    count))

(s/fdef all-authors
        :args (s/cat :ds ::parsers/vcs-log
                     :options map?)
        :ret ::core/analysis-result)
(defn all-authors
  [ds _options]
  (let [vs (->> ds
                (map (juxt :author :author-email))
                (sort-by :author)
                distinct)]
    {:analysis-result-header [:author :author-email]
     :analysis-result-values vs}))

(defn active-authors
  [commits {:keys [cut-off-age-for-active-author] :or {cut-off-age-for-active-author (tc/months 3)}}]
  (if (seq commits)
    (let [by-authors (group-by :author commits)
          stats (map (fn [ds]
                       {:last-contrib-time (second (authors-shared/contribution-time (second ds)))
                        :author (first ds)})
                     by-authors)
          by-last-contribution (->> stats (sort-by :last-contrib-time) reverse)
          last-contrib-date (authors-shared/as-time (:last-contrib-time (first by-last-contribution)))
          cut-off-date (when last-contrib-date
                         (tc/minus last-contrib-date cut-off-age-for-active-author))]
      (->> by-last-contribution
           (remove (fn [{:keys [last-contrib-time]}]
                     ;; if there is no cut-off-date, there are no contributions at all,
                     ;; and so we consider every author an non-active author
                     (or (nil? cut-off-date)
                         ;; the normal case where there are contributions and a
                         ;; cut-off-date
                         (tc/before? (authors-shared/as-time last-contrib-time) cut-off-date))))
           (map :author)))
    [])) ; no commits, no active authors (happens if start time is ahead of the last commit or empty repo)


