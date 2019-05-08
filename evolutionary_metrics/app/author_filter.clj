;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.app.author-filter
  (:require [evolutionary-metrics.app.authors-by-team :as team-reader]))

(defn- remove-from
  [keep? commits]
  (filter (comp keep? :author) commits))

(defn keep-known-authors
  [commits authors]
  (remove-from authors commits))

(defn filter-specified-authors
  [commits authors-to-exclude]
   (remove-from (fn [author-name] (nil? (authors-to-exclude author-name))) commits))

(defn- with-team-for
  [teams author]
  (let [team (get teams author "Unknown")]
    (str team "." author)))

(defn- as-authors-by-team
  [commits author->team-fn options]
  (if (:augment-author-teams options)
    (map (fn [{:keys [author] :as c}]
           (assoc c :author (author->team-fn author)))
         commits)
    commits))

(defn- filter-excluded-authors-from
  [commits user-option options]
  (let [author->team (team-reader/authors-by-team user-option)
        team-augmenter-fn (partial with-team-for author->team)]
    (if (seq author->team)
      (-> commits
          (keep-known-authors author->team)
          (as-authors-by-team team-augmenter-fn options))
      commits))) ; no known authors -> include everyone!

(defn by-author-names
  "Filters the given commits based on a set of authors.
   The idea is to specify all authors of interest in a file and 
   get rid of the rest.
   This functionality is typically used to include people who 
   only do merges or as a way of analyzing the activity by 
   a single team only"
  [options commits]
  (if-let [user-option (:known-authors options)]
    (filter-excluded-authors-from commits user-option options)
    commits))

(defn by-excluded-author-names
  "Filters the given commits based on the specified author names.
   authors-to-exclude : a set of names.
   Note that this function is the reverse to by-author-names - here
   we filter all specified, there we include all specified authors."
  [options commits]
  (if-let [authors-to-exclude (:excluded-authors options)]
    (filter-specified-authors commits authors-to-exclude)
    commits))
 
