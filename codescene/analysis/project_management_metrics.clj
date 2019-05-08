(ns codescene.analysis.project-management-metrics
  (:require [codescene.analysis.closed-maat-proxy :as maat]
            [codescene.analysis.paths :as paths]
            [codescene.analysis.pm-management-specs :as pm-specs]
            [clojure.spec.alpha :as s]
            [cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.string :as string]
            [evolutionary-metrics.analysis.costs :as costs]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [evolutionary-metrics.parsers.identity :as identity-parser]
            [evolutionary-metrics.trends.cost-trend :as costs-trend]
            [evolutionary-metrics.trends.work-trend :as work-trend]
            [taoensso.timbre :as log]))

(def item-id-is-commit-hash 1)
(def item-id-is-ticket-id 2)

(defn use-project-management-metrics?
  [project]
  (get-in project [:pm-integration :enabled?]))

;;
;; Data mining section
;; ===================

(defn- query-api
  ([repo path]
    (query-api {} repo path))

  ([options
    {{:keys [api-url
             api-username
             api-password]} :pm-integration}
    path]
    (let [request-options (merge
                            (when (and api-username api-password)
                              {:basic-auth [api-username api-password]
                               :socket-timeout 5000
                               :conn-timeout 5000})
                            options)
          url (str api-url path)
          ;; client/get validates the url
          {:keys [body]} (client/get url request-options)]
      (json/parse-string body true))))

(defn- costs-units-from
  [projects]
  (->> projects
       (map (comp :type :costUnit))
       (into #{})))

(defn- supported-cost-unit-types?
  [projects]
  (let [u (->> projects costs-units-from (into #{}))]
    (and (= 1 (count u))
         (clojure.set/subset? u #{"minutes" "points"}))))

(defn- error-message-for-invalid-cost-units
  [cost-units]
  (if (some nil? cost-units)
    "Incorrect PM project configuration: make sure you have configured the correct Ticket/Task IDs in both the (Jira) plugin and CodeScene"
    (str "Incompatible PM projects discovered. All of them MUST use the same cost unit, but you have: " (pr-str cost-units))))

(defn fail-when-incompatible-pm-sources
  "In case the user configures multiple PM data sources, all those
   projects have to agree upon what kind of cost they represent.
   For example, mixin a cost in hours with another project that uses
   story points isn't a receipt for success."
  [projects]
  (if (supported-cost-unit-types? projects)
    projects
    (let [cost-units (costs-units-from projects)
          error-message (error-message-for-invalid-cost-units cost-units)]
      (log/error error-message)
      (throw (Exception. error-message)))))

(defn- log-project-input
  [projects]
  (doseq [{:keys [id workTypes items]} projects]
    (taoensso.timbre/info "Import PM data from project '" id "' with work types: " (pr-str workTypes) " and " (count items) " items."))
  projects)

(defn get-status [repo]
  (query-api repo "/api/1/status"))

(s/fdef get-projects
        :ret ::pm-specs/pm-projects)

(defn get-projects [{{:keys [parsed-external-project-ids]} :pm-integration :as repo}]
  (taoensso.timbre/info "Fetching PM metrics from the following projects: " (pr-str parsed-external-project-ids))
  (doall
    (->> parsed-external-project-ids
         (map #(query-api repo (str "/api/1/projects/" %)))
         log-project-input
         fail-when-incompatible-pm-sources)))

(defn- id-type->enum [id-type]
  (condp = id-type
    "ticket-id" item-id-is-ticket-id
    item-id-is-commit-hash))

(s/fdef cost-by-identifiable-item
        :args (s/cat :projects ::pm-specs/pm-projects)
        :ret ::pm-specs/cost-by-item)

(defn- formatted-cost-items-from
  [items]
  (mapv (juxt :id :cost) items))

(defn cost-by-identifiable-item
  "Creates a seq with the cost of implementing a specific feature, bug, etc. Note that the
   feature ('identifiable-item') ID can refer to either Ticket IDs or commit hashes.
   It's configurable in the integration service which one we'll use and include in the
   response here."
  [projects]
  (if (not (empty? projects))
    (let [{:keys [idType costUnit]} (first projects)
          combined-items (->> projects (map :items) (mapv formatted-cost-items-from) (apply concat))]
      {:id-type   (id-type->enum idType)
       :cost-unit costUnit
       :items      combined-items})
    []))

(defn- unify-work-types
  [all-types]
  (reduce (fn [acc types]
            (let [overlapping (clojure.set/intersection (set acc) (set types))
                  unique (remove overlapping types)]
              (concat acc unique)))
          all-types))

(defn- adapt-single-item-to-work-types
  [all-work-types my-work-types my-identifiable-work-type]
  (let [my-id (first my-identifiable-work-type)
        my-work-spec (rest my-identifiable-work-type)
        my-spec (into {} (map vector my-work-types my-work-spec))]
    (assert (= (count my-work-spec) (count my-work-types)))
    (->> all-work-types
         (map #(get my-spec % 0))
         (concat [my-id]))))

(defn adapt-to-shared-work-types
  [all-work-types labels items]
  (map (fn [label project-items]
         (->> project-items
              (map (comp vec flatten (juxt :id :types)))
              (mapv (partial adapt-single-item-to-work-types all-work-types label))))
       labels
       items))

(defn- unify-items
  [all-work-types projects]
  (let [labels (map :workTypes projects)
        items (map :items projects)]
    (assert (= (count labels) (count items)))
    (apply concat (adapt-to-shared-work-types all-work-types labels items))))

(s/fdef type-of-work-by-identifiable-item
        :args (s/cat :projects ::pm-specs/pm-projects)
        :ret ::pm-specs/type-of-work-items)

(defn type-of-work-by-identifiable-item
  "Maps a PM project response to a table that specifies the type of work behind each
   identifiable item. This data is used to look at trends like
   an increase in maintenance activity inside a Hotspot.

   NOTE: The actual types are unknown here - it's a responsibility
   of the PM integration to interpret them and, possibly, combine
   multiple labels into a shared type (e.g. 'Bug' and 'Refactoring' may both
   be combined into 'Maintenance')."
  [projects]
  (if (not (empty? projects))
    (let [{:keys [idType]} (first projects)
          all-work-types (unify-work-types (map :workTypes projects))]
      {:id-type (id-type->enum idType)
       :supported-types  all-work-types
       :items-with-types (unify-items all-work-types projects)})
    []))

;;
;; Analysis section
;; ================

;; TODO: we need to carry around the id-type retrieved from JIRA in the analysis context, because
;; we need the id-type here to decide upon the analysis strategy.
(defn- id-type-strategy-from
  [{:keys [ticketidpattern]}]
  {:temporal-by-ticket-id      1                            ; TODO: rename to make it more general.
   :message-extraction-pattern ticketidpattern})

(defn analyze-costs-of
  [{:keys [filtered-log analysis-path-fn code-maat-params pm-costs-log]} project dest-file-name]
  (when (use-project-management-metrics? project)
    (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))
          analysis-params (merge code-maat-params (id-type-strategy-from project) {:costs-log-name pm-costs-log})]
      (maat/run-closed-maat filtered-log "costs" destination analysis-params))))

(defn analyze-architectural-costs-of
  [{:keys [architectural-log analysis-path-fn code-maat-params pm-costs-log]} project dest-file-name]
  (when (use-project-management-metrics? project)
    (let [destination (analysis-path-fn (paths/analysis->result-path dest-file-name))
          analysis-params (merge code-maat-params (id-type-strategy-from project) {:costs-log-name pm-costs-log})]
      (maat/run-closed-maat architectural-log "costs" destination analysis-params))))

(defn- top-spots-for-trend-calculation
  [{:keys [analysis-path-fn]}
   {:keys [n-complexity-trends] :as _complexity-warning-thresholds}
   hotspot-file]
  (->> (analysis-path-fn hotspot-file)
       shared/read-csv-sans-header-from
       (take n-complexity-trends)
       (map first)))                                        ; name field

(defn- generate-evo-log-resolved-by-id-type-for-file-level
  "We don't want to resolve the same IDs over and over again for each trend, so
   let's generate a log with the information. Perhaps this should be in the mining stage?"
  [{:keys [filtered-log analysis-path-fn code-maat-params]} project]
  (let [destination (analysis-path-fn (paths/analysis->result-path paths/pm-resolved-log))
        analysis-params (merge code-maat-params (id-type-strategy-from project))]
    (maat/run-closed-maat filtered-log "identity" destination analysis-params)
    (identity-parser/parse-log destination {})))

(defn- generate-evo-log-resolved-by-id-type-for-arch-level
  [{:keys [architectural-log analysis-path-fn code-maat-params]} project]
  (let [destination (analysis-path-fn (paths/analysis->result-path paths/pm-arch-resolved-log))
        analysis-params (merge code-maat-params (id-type-strategy-from project))]
    (maat/run-closed-maat architectural-log "identity" destination analysis-params)
    (identity-parser/parse-log destination {})))

(defn- use-default-when-missing
  [v]
  (if (empty? v)
    ["Unknwon"]
    v))

(defn- work-types-in
  [header]
  (->> header
       (drop 1)                                             ; ID field
       (into [])
       use-default-when-missing))

(defn- as-work-type-by-id
  [rows]
  (->> rows
       (map (fn [[id & types]] [id (map #(Integer/parseInt %) types)]))
       (into {})))

(defn calculate-specific-costs-trends
  "This function generates data for two related trend curves:
    - cost: hours/points/etc spent per month
    - type of work: hours spent on bugs/features/etc per month

   The function is used for both file- and architectural level cost trends."
  [{:keys [pm-costs-log pm-type-of-work-log time-now] :as context}
   project
   complexity-warning-thresholds
   {:keys [result-root-path hotspot-result-file commits-ds]}]
  (when (use-project-management-metrics? project)
    (let [top-spots (top-spots-for-trend-calculation context complexity-warning-thresholds hotspot-result-file)
          costs-by-id (costs/as-costs-by-id pm-costs-log)
          type-of-work (shared/read-csv-from pm-type-of-work-log)
          work-types (work-types-in (first type-of-work))
          work-type-by-id (as-work-type-by-id (rest type-of-work))]
      (log/trace "Generating costs trends for: " (string/join ", " top-spots))
      (costs-trend/costs-trend-for commits-ds costs-by-id top-spots time-now result-root-path)
      (log/trace "Generating work type trends for the following work items: " (string/join ", " work-types))
      (work-trend/work-trends-for commits-ds costs-by-id work-type-by-id work-types top-spots result-root-path))))

(defn generate-file-level-costs-trends
  [{:keys [analysis-path] :as context}
   project
   complexity-warning-thresholds]
  (let [result-root-path (paths/as-child-path analysis-path paths/costs-trend-path-name)
        commits-ds (generate-evo-log-resolved-by-id-type-for-file-level context project)]
    (calculate-specific-costs-trends context project complexity-warning-thresholds
                                     {:result-root-path result-root-path
                                      :hotspot-result-file paths/pm-cost-hotspots-csv
                                      :commits-ds commits-ds})))

(defn generate-arch-level-costs-trends
  [{:keys [analysis-path] :as context}
   project
   complexity-warning-thresholds]
  (let [result-root-path (paths/as-child-path analysis-path paths/architectural-costs-trend-path-name)
        commits-ds (generate-evo-log-resolved-by-id-type-for-arch-level context project)]
    (calculate-specific-costs-trends context project complexity-warning-thresholds
                                     {:result-root-path result-root-path
                                      :hotspot-result-file paths/pm-cost-hotspots-by-logical-component-csv
                                      :commits-ds commits-ds})))

(defn generate-costs-trends
  "This function generates data for two related trend curves:
    - cost: hours/points/etc spent per month
    - type of work: hours spent on bugs/features/etc per month

    'date-now' should be a DateTime

   The analysis is run on both file and architectural level."
  [context
   project
   complexity-warning-thresholds]
  (when (use-project-management-metrics? project)
    (generate-file-level-costs-trends context project complexity-warning-thresholds)
    (generate-arch-level-costs-trends context project complexity-warning-thresholds)))
