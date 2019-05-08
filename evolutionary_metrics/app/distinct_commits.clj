;;; Copyright (C) 2014-2015 Adam Tornhill
;;;

(ns evolutionary-metrics.app.distinct-commits
  (:require [medley.core :as m]))

(defn distinct-commits
  "Selects distinct commits based on date, author, message and entity."
  [options commits]
  (if (:select-distinct-commits options)
    (m/distinct-by (juxt :date :author :entity :message) commits)
    commits))
