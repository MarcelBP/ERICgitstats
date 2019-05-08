;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.app.team-mapper
  (:require [evolutionary-metrics.app.authors-by-team :as team-reader]))

(defn- team-for
  [default-team-name teams author]
  (get teams author default-team-name))

(defn- authors->teams
  [commits author->team-fn]
  (map (fn [{:keys [author] :as c}]
         (assoc c :author (author->team-fn author)))
       commits))

(defn commit-authors->teams
  [commits default-team-name teams]
  (->> teams
       (partial team-for default-team-name)
       (authors->teams commits)))

(defn- author->team-based-on
  [commits user-team-map default-team-name]
  (commit-authors->teams commits
                         default-team-name
                         (team-reader/authors-by-team user-team-map)))

(defn- default-team-name-from
  [{:keys [unknown-team-name]}]
  (or unknown-team-name "NoTeam"))

(defn by-teams
  "Transforms all authors to their team names.
   This allows us to analyze on a team level instead 
   of individual contributors.
   Useful on its own, but also in organizations that 
   make heavy use of squash commits for multiple contributors 
   at once."
  [options commits]
  (if-let [user-team-map (:team-map options)]
    (author->team-based-on commits user-team-map (default-team-name-from options))
    commits))
 
