;;; Copyright (C) 2014-2015 Adam Tornhill
;;;

(ns evolutionary-metrics.app.time-based-grouper
  (:require [medley.core :as medley]))

;;; Sometimes we'd like to use a different temporal window than
;;; the commit. For example, when multiple teams are involved
;;; changes may have to be done in multiple commits to multiple repositories.
;;; We'd like to re-group them.

;;; Further, some organizations use a workflow that involves
;;; many small commits.
;;; To remove these biases we use this module to re-group all
;;; changes according to a given time window before analysis.
;;;
;;; LIMITATION: At the moment we only support grouping commits that
;;; occour within the same day. This is because I could implement
;;; that aggregation easily. It would be nice to have a rolling window
;;; with a configurable width instead, but it introduces a set of complications:
;;;  - Commits will be included multiple times in different windows.
;;;  - We don't know when each programmer leaves work and will miss things
;;;    that belong together, but are produced on different days (not that we
;;;    solve that today either, but at least we don't pretend to).

(defn- join-commits
  [[ks commits-on-a-day]]
  (let [[date author] ks
        joined-rev (str date author)]
    (->> commits-on-a-day
         (medley/distinct-by :entity)
         (map (fn [m] (assoc m :rev joined-rev))))))

(defn- join-by-author-and-day
  "Alright, this is a hack: we set the commit ID to
   the current date + the name of the author. That makes the rest of the analyses treat
   our faked grouping as beloning to the same change set."
  [commits]
  (->> commits
       (group-by (juxt :date :author))
       (map join-commits)
       flatten))

(defn by-same-author-on-same-day
  "Groups individual commits into aggregates based on
   the given temporal period and the name of the author. "
  [options commits]
  (if (:temporal-by-author-and-day options)
    (join-by-author-and-day commits)
    commits))
