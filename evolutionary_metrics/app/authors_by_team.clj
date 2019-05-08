;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.app.authors-by-team
  (:require [evolutionary-metrics.mergers.shared-mergers :as shared]))

(defn- author-and-team-from
  [[author _ team]]
  [author team])

(defn authors-by-team
  "Expects a CSV file with the author name as 
   the first column team name as the third (the 
   second one is typically reserved for color)"
  [authors-file-name]
  (->> authors-file-name
       shared/read-csv-sans-header-from
       (map author-and-team-from)
       (into {})))