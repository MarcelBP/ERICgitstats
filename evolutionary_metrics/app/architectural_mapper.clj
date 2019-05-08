(ns evolutionary-metrics.app.architectural-mapper
  (:require [evolutionary-metrics.mergers.shared-mergers :as shared]
            [evolutionary-metrics.app.glob-patterns :as glob]))

;;; This transformation supports analyses according to pre-defined architectual groups.
;;; These groups are typically architectural boundaries. All data
;;; will be aggregated into that view before analysis.
;;;
;;; The transformation specification is given as a CSV file with two columns:
;;; 1. Match pattern: this is a glob pattern that specifies the rule for matching specific content to the architectural name.
;;; 2. Architectural name: once the associated glob pattern matches, all matching content will be transformed to this name.


(defn as-group-specification
  [[path name]]
  {:match-pattern path
   :architectural-name name})

(defn text->group-specification
  "Transforms the given text into a
   seq of maps specifying the grouping.
   Each map will have the following content:
    {:path 'some/path' :name 'some_name'}"
  [input]
  (map as-group-specification
       (shared/read-csv-sans-header-from input)))

;; Mapping physical entities to logical groups
;; ===========================================

(defn entity->architectural-name
  [entity group-exprs]
  (some->> group-exprs
           (some (fn [{:keys [match-pattern] :as m}] (and (glob/glob-match match-pattern entity) m)))
           :architectural-name))

(defn within-group?
  [group-exprs entity]
  (some->>
    (map :match-pattern group-exprs)
    (some #(glob/glob-match % entity))))

(defn map-entities->groups
  "Maps each entity in the commits to one of the pre-defined
   architectural boundaries (groups)."
  [commits group-exprs]
  (->> commits
       (filter (comp (partial within-group? group-exprs) :entity))
       (map (fn [{:keys [entity] :as m}]
              (assoc m :entity (entity->architectural-name entity group-exprs))))))

(defn files->architectural-boundary
  "This entry point parses the given group info.
   All entities in each commit are then re-mapped to one
   of the given groups."
  [group-info-file commits]
  (map-entities->groups
    commits
    (text->group-specification group-info-file)))

(defn aggregate-on-boundaries
  "The individual changes may be aggregated into layers
   of architectural significance. This is done by re-mapping
   the name.
   In case there isn't any specified grouping, just work on
   the raw commits."
  [options commits]
  (if-let [grouping (:group options)]
    (files->architectural-boundary grouping commits)
    commits))

(defn valid-match-pattern?
  [pattern-expression]
  (try
    (do (re-pattern (glob/glob->regex pattern-expression) )
        (not (empty? pattern-expression)))
    (catch Exception _ false)))
