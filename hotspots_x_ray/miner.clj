(ns hotspots-x-ray.miner
  (:require [evolutionary-metrics.mining.vcs :as vcs]
            [hotspots-x-ray.detectors.change-detector :as detector]
            [hotspots-x-ray.diagnostics.performance :as diagnostics]
            [taoensso.timbre :as timbre]))

(defn rename-functions
  [rename-context changeset]
  (let [changes (:changes changeset)
        renamed (filter (comp rename-context :name) changes)
        same-names (remove (comp rename-context :name) changes)
        new-names (map (fn [{:keys [name] :as change}] (assoc change :name (get rename-context name))) renamed)]
    (->> new-names
         (concat same-names)
         (sort-by :name) ; just to make the unit tests deterministic
         (assoc changeset :changes))))

(defn- extend-with-rename
  [rename-context {:keys [old-name new-name]}]
  (assoc rename-context old-name new-name))

(defn- renames-to-replace
  "Returns the keys in the rename context that match
   the candidate replacements."
  [rename-context candidates]
  {:pre [(map? rename-context) (set? candidates)]}
  (->> rename-context
       (filter (fn [[_k v]] (candidates v)))
       (map first))) ; return the keys

(defn update-rename-context
  "The rename context is a map of old-name => new-name.
   We traverse the history backwards so as soon as we
   notice a new rename for one of the entries in the rename-context,
   we need to update the context with the new replacement."
  [rename-context {:keys [renames rev date] :as _changeset}]
  {:pre [(map? rename-context)]}
  (let [possible-rename-replacements (->> renames (map :old-name) (into #{}))
        rename-replacements (renames-to-replace rename-context possible-rename-replacements)
        cleaned-context (reduce dissoc rename-context rename-replacements)]
    (timbre/trace "X-Ray: renames detected in revision <" rev "> on " date ":"  (pr-str rename-replacements) " with " (pr-str renames))
    (reduce extend-with-rename cleaned-context renames)))

(defn- rename-reducer
  [[resolved rename-context] changeset]
  [(conj resolved (rename-functions rename-context changeset))
   (update-rename-context rename-context changeset)])

(defn- resolve-renames
  [changes-by-date]
  (->> changes-by-date
       reverse ; traverse newest to oldest
       (reduce rename-reducer [[] {}])
       first
       reverse ; back to oldest first
       (map #(dissoc % :renames))))

(defn- sliding-revisions-window
  [revisions]
  (partition 2 1 revisions))

(defn- detect-raw-changes-across
  "Detects all changed functions without resolving renames.
   The rename contexual information is returned as part of the result."
  [file-name revisions]
  (map (partial detector/changes-between file-name)
       (sliding-revisions-window revisions)))

(defn detect-changes-across
  [file-name revisions]
  (diagnostics/with-timbre-exe-time-info
    (->> revisions
         (detect-raw-changes-across file-name)
         resolve-renames)))

(defn limit-to-configured-threshold
  [{:keys [max-historic-revisions]} revs]
  (if (some? max-historic-revisions)
    (->> revs reverse (take max-historic-revisions) reverse)
    revs))

(defn- revisions-of
  [repo start-date file-name git-cmd thresholds]
  (diagnostics/with-timbre-exe-time-info
    (->>
      (vcs/historic-file-versions-metadata git-cmd repo start-date file-name)
      (limit-to-configured-threshold thresholds)
      (map (partial vcs/historic-file-version-of git-cmd repo))
      (filter (partial first)) ; filter away invalid versions
      (map second)))) ; drop the state of the diff now that only valid versions remain

(defn evolutionary-metrics-for
  [repo start-date file-name git-cmd thresholds]
  ;; delivered in order oldest first
  (detect-changes-across
   file-name
   (revisions-of repo start-date file-name git-cmd thresholds)))
