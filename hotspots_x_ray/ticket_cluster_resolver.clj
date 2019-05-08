(ns hotspots-x-ray.ticket-cluster-resolver
  (:require [evolutionary-metrics.mining.file-patterns :as file-patterns]
            [hotspots-x-ray.x-rayer :as x-rayer]
            [hotspots-x-ray.cluster-resolver :as cluster-resolver]
            [evolutionary-metrics.mining.vcs :as vcs]
            [hotspots-x-ray.miner :as miner]
            [hotspots-x-ray.x-rayer :as x-rayer]
            [semantic-csv.core :as sc]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.core :as evo-specs]
            [evolutionary-metrics.mining.specs :as mining-specs]
            [evolutionary-metrics.app.ticket-grouper :as ticket]
            [hotspots-x-ray.diagnostics.performance :as diagnostics]
            [hotspots-x-ray.recommendations.coupling :as coupling]))

(defn- history-by-ticket-id
  [ticket-id-options hs]
  (ticket/by-ticket-id ticket-id-options hs))

(defn relevant-history-with-ticket-ids
  [ticket-id-options coupling-cluster full-history]
  (let [files-of-interest (map (fn [{:keys [repo file-name]}] (str (file-patterns/final-segment repo) "/" file-name)) coupling-cluster)]
    (->> full-history
         (filter (comp (set files-of-interest) :entity))
         (history-by-ticket-id ticket-id-options))))

(defn history-by-file
  [ticket-id-options coupling-cluster full-history]
  (->> full-history
       (relevant-history-with-ticket-ids ticket-id-options coupling-cluster)
       (group-by :entity)))

#_(defn- functions-in-revision
  [[k v]]
  (let [fs (->> v (map :entity) flatten)]
    {:changes fs :rev k}))

;; TODO: algorithm
; 1. All revisions with Ticket IDs for a file
; 2. Resolve changed functions by Ticket ID.
; 3. Reduce to one changeset per Ticket ID (called rev).
; 4. Rest is same as cluster resolver...

(defn- code-in-revisions-of
  "Returns all revisions of interest (the ones we have TICKET IDs for) of
   the given file name. The results are sorted on their author date."
  [repo start-date file-name git-cmd thresholds revisions-of-interest]
  (diagnostics/with-timbre-exe-time-info
    (->>
      (vcs/historic-file-versions-metadata git-cmd repo start-date file-name)
      (filter (comp revisions-of-interest :rev))
      (miner/limit-to-configured-threshold thresholds)
      (map (partial vcs/historic-file-version-of git-cmd repo))
      (filter (partial first)) ; filter away invalid versions
      (map second)))) ; drop the state of the diff now that only valid versions remain

(defn- code-by-ticket-id
  [revision->ticket-id code-by-revision]
  (map (fn [{:keys [rev] :as c}]
         (assoc c :rev (revision->ticket-id rev)))
       code-by-revision))

(defn combine-evolution-for-same-ticket-id
  "Changes done in different commits but refering to the same Ticket ID have
   to be comined into a single, logical change."
  [cs]
  (->> cs
       (reduce (fn [acc {:keys [rev] :as change}]
                 (assoc acc rev
                            (if-let [existing (get acc rev)]
                              (let [newer-version (->> change :changes (map :name) set) ; based on the assumption that we run on sorted data, oldest first
                                    stripped-change (remove (comp newer-version :name) (:changes existing))]
                                (assoc existing :changes (concat stripped-change (:changes change))))
                              change)))
               {})
       vals
       (sort-by :date)
       reverse))

(defn- evolutionary-metrics-for
  [repo-lookup start-date git-cmd thresholds single-file-history]
  (let [[file-name hs] single-file-history
        [repo-name real-file-name] (file-patterns/de-scope file-name)
        repo (repo-lookup repo-name)
        revisions-of-interest (->> hs (map :original-rev) set)
        code-by-rev (code-in-revisions-of repo start-date real-file-name git-cmd thresholds revisions-of-interest)
        revision->ticket-id (->> hs (map (juxt :original-rev :rev)) (into {}))
        code-by-ticket (->> code-by-rev (code-by-ticket-id revision->ticket-id) combine-evolution-for-same-ticket-id)
        changes-across-tickets (miner/detect-changes-across real-file-name code-by-ticket)
        analyzable (x-rayer/as-analyzable-evolution repo real-file-name changes-across-tickets)]
    (cluster-resolver/internal-temporal-coupling-analysis-format file-name analyzable)))

(defn- as-maat-options
  [ticket-id-pattern]
  {:temporal-by-ticket-id 1
   :message-extraction-pattern ticket-id-pattern})

(s/def ::repo ::evo-specs/existing-path)
(s/def ::file-name ::evo-specs/filename)

(s/def ::coupled-file (s/keys :req-un [::repo ::file-name]))
(s/def ::coupling-cluster-seq (s/and (s/coll-of ::coupled-file)
                                     #(<= 2 (count %)))) ; at least two files in temporal coupling cluster

(defn resolve-coupling-by-ticket-for
  [coupling-cluster parsed-git-log-csv-file ticket-id-pattern start-date git-cmd thresholds out-file-name]
  (let [repo-lookup (->> coupling-cluster (map :repo) file-patterns/make-scope->repo-lookup)
        options (as-maat-options ticket-id-pattern)
        history (->> parsed-git-log-csv-file sc/slurp-csv (history-by-file options coupling-cluster))
        ticket-evolution-by-file (map (partial evolutionary-metrics-for repo-lookup start-date git-cmd thresholds) history)]
    (coupling/coupled-functions-to-csv ticket-evolution-by-file thresholds out-file-name)
    out-file-name))

(s/fdef resolve-coupling-by-ticket-for
        :args (s/cat :coupling-cluster ::coupling-cluster-seq
                     :parsed-git-log-csv-file ::evo-specs/existing-path
                     :ticket-id-pattern string? ; could be anything...
                     :start-date ::evo-specs/date-string
                     :git-cmd ::mining-specs/git-exe-command
                     :thresholds (s/map-of keyword? nat-int?)
                     :out-file-name ::evo-specs/filename)
        :ret ::evo-specs/existing-path)
