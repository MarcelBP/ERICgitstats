;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.app.content-filter
  (:require [evolutionary-metrics.mining.file-patterns :as patterns]
            [evolutionary-metrics.mergers.shared-mergers :as shared]))

(defn- as-filter
  [user-option]
  (-> user-option
      patterns/user-option->patterns
      patterns/patterns->combined-name-filter-fn))

(defn- remove-from
  [keep? commits]
  (filter(comp keep? :entity) commits))

(defn- filter-excluded-files-from
  [commits user-option]
  (-> user-option
      as-filter
      (remove-from commits)))

(defn by-glob-patterns
  "Filters the given commits based on a set of glob patterns 
   given in the options. A user may specify multiple patterns, 
   each separated by a semicolon.
   The rationale is to filter files that we want to exclude from 
   an analysis. For example, in Java we don't want jar's or class'es.
   Sometimes we even want to exclude build related scripts like 
   Maven and Ant scripts."
  [options commits]
  (if-let [user-option (:exclude-files options)]
    (filter-excluded-files-from commits user-option)
    commits))

;;
;; Filter by a given input file
;;

(defn- as-presence-filter
  [filter-file-name]
  (let [present-files (shared/modules-with-complexity filter-file-name)
        present-names (set (map first present-files))]
    (fn [name]
      (get present-names name))))

(defn- filter-present-files-in
  [commits filter-file-name]
  (remove-from (as-presence-filter filter-file-name) commits))

(defn by-input-file
  "Filters the given commits based on the existence of each file in
   the given filter file.
   This functionality is typically used to exclude files that are no
   longer present in the current repository (e.g. we don't want temporal
   coupling of historic files."
  [options commits]
  (if-let [user-option (:filter-specified-files options)]
    (filter-present-files-in commits user-option)
    commits))
