(ns codescene.note-watch.renames
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.data.csv :as csv]
            [clojure.spec.alpha :as s]
            [cheshire.core :as json]
            [codescene.analysis.specs :as specs]
            [codescene.note-watch.specs :as note-specs]
            [jordanlewis.data.union-find :as uf]
            [taoensso.timbre :as log]
            [codescene.analysis.paths :as paths]
            [evolutionary-metrics.parsers.git-commit-parser :as commit-parser]))

(defn- write-default-intermediate-rename-file
  "An edge case where we have zero commits to analyse. Create an empty file
   to avoid special cases later in the analysis pipeline."
  [intermediate-log-destination]
  (with-open [out-file (io/writer intermediate-log-destination)]
    (csv/write-csv out-file commit-parser/parsed-git-entry-headers)))

(defn make-write-intermediate-results-for-renaming-fn
  [intermediate-log-destination]
  (fn [commits]
    (if-let [commit-keys (->> commits (mapcat keys) set seq)]
      (do
        (log/debug "Persisting intermediate results for renaming: " intermediate-log-destination commit-keys)
        (with-open [out-file (io/writer intermediate-log-destination)]
          (csv/write-csv out-file [(into [] (map name) commit-keys)])
          (csv/write-csv out-file (map (apply juxt commit-keys) (reverse commits)))))
      (write-default-intermediate-rename-file intermediate-log-destination))
    commits))

;; We don't assume that {a => b} could be directory name we always take it as a renaming rule
;; /a/.../{directory name}/c/.../{a => b}/d/... is not handled correctly
(defn- get-old-n-new-entity-name
  "Parse entity renaming and returns old and new entity name"
  [entity-name]
  (let [res (re-seq #"\{(.*?) => (.*?)\}" entity-name)]
    (if (nil? res)
      (mapv string/trim (string/split entity-name #" => "))
      (->> res
           (reduce (fn [[old-str new-str] [match old new]]
                     [(string/replace-first old-str match old)
                      (string/replace-first new-str match new)])
                   [entity-name entity-name])
           (mapv #(string/replace % #"\/+" "/"))))))

(defn- entity->unique-name
  [entity rev]
  (str entity "+$$$+" rev))

(defn- get-unique-name
  [same-names-remap entity]
  (let [revs (get same-names-remap entity)]
    (if revs
      (if (= (count revs) 1)
        entity
        (entity->unique-name entity (last revs)))
      entity)))

(defn- get-root
  [rename-graph name]
  (let [graph-n-root (uf/get-canonical rename-graph name)]
    (if (get graph-n-root 1)
      graph-n-root
      [(get graph-n-root 0) name])))

(defn- perform-renaming
  [{:keys [rename-graph same-names-remap] :as stat} {:keys [entity rev]}]
  (let [[old-name new-name] (get-old-n-new-entity-name entity)
        unique-old-name (get-unique-name same-names-remap old-name)
        [new-rename-graph root] (-> rename-graph
                                    (get-root unique-old-name)
                                    (as-> x
                                        [(-> x (get 0) (conj new-name) (uf/union (get x 1) new-name)) (get x 1)]))]
    (-> stat
        (assoc :rename-graph new-rename-graph)
        (update :current-names assoc root new-name)
        (update-in [:same-names-remap root] #(if (seq %) % [rev]))
        (update-in [:same-names-remap new-name] #(if (seq %) % [rev])))))

(defn- check-n-perform-rename-collision
  [{:keys [rename-graph current-names same-names-remap] :as stat} {:keys [entity rev]}]
  (let [unique-name (get-unique-name same-names-remap entity)
        [rename-graph1 root] (get-root rename-graph unique-name)
        current-name (get current-names root)
        new-name (if (or (nil? current-name) (= current-name entity))
                   unique-name
                   (entity->unique-name entity rev))
        rename-graph2 (conj rename-graph1 new-name)
        new-current-names (if (= current-name entity)
                            current-names
                            (assoc current-names new-name entity))
        new-same-names-remap (if (= current-name entity)
                               same-names-remap
                               (update same-names-remap entity (fnil conj []) rev))]
    (-> stat
        (assoc :rename-graph rename-graph2)
        (assoc :current-names new-current-names)
        (assoc :same-names-remap new-same-names-remap))))

(defn- update-traces
  [labels {:keys [commits-order last-rev-idx] :as stat} line]
  (let [log (zipmap labels line)
        old-rev-idx (get commits-order (:rev log))
        rev-idx (or old-rev-idx last-rev-idx)
        new-stat (if (-> log :extraction-name empty?)
                   (check-n-perform-rename-collision stat log)
                   (perform-renaming stat log))]
    (-> new-stat
        (update :last-rev-idx #(if old-rev-idx % (inc %)))
        (update :commits-order #(if old-rev-idx % (assoc % (:rev log) rev-idx))))))

(defn build-traces
  [cvs-data & [traces]]
  (let [labels (->> cvs-data first (map keyword))]
    (reduce (partial update-traces labels)
            (or traces
                {:commits-order {}
                 :rename-graph (uf/union-find)
                 :same-names-remap {}
                 :current-names {}
                 :last-rev-idx 1})
            (rest cvs-data))))

(defn write-traces!
  [file-name traces]
  (binding [*print-length* -1]
    (with-open [out-file (io/writer file-name)]
      (.write out-file
              (-> traces
                  (update :rename-graph #(group-by % (keys (.-elt-map %))))
                  prn-str))))
  traces)

(defn- conj-childrent-to-root
  [graph [root children]]
  (reduce (fn [new-graph child]
            (-> new-graph
                (conj child)
                (uf/union root child)))
          (conj graph root) children))

(defn read-traces
  [file-name]
  (with-open [in-file (io/reader file-name)]
    (let [traces (edn/read (java.io.PushbackReader. in-file))]
      (update traces :rename-graph
              (fn [graph-map]
                (reduce conj-childrent-to-root
                        (uf/union-find) graph-map))))))

(s/def ::commits-order map?)
(s/def ::current-names map?)
(s/def ::same-names-remap map?)
(s/def ::rename-graph #(instance? jordanlewis.data.union_find.PersistentDSF %))

(s/def ::traces (s/keys :req-un [::commits-order
                                 ::current-names
                                 ::same-names-remap
                                 ::rename-graph]))

(s/fdef build-traces-from-file
        :args (s/alt :unary (s/cat :intermediate-log-destination ::specs/path)
                     :binary (s/cat :intermediate-log-destination ::specs/path
                                    :traces ::traces))
        :ret ::traces)

(defn build-traces-from-file
  "Build data structure for tracing names changes during history.

  :commits-order map rev-hash -> int (linear order of revisions as were in file - we are on one branch)
  :current-names map root-name (beginnings of rename paths) -> current name
  :same-names-remap map file-name -> [rev hash when introduced this name]
  :rename-graph union-find structure representing graph of renaming"
  [intermediate-log-destination & [traces]]
  (with-open [in-file (io/reader intermediate-log-destination)]
    (let [csv-data (csv/read-csv in-file)]
      (build-traces csv-data traces))))

(s/fdef new-name-for
        :args (s/cat :traces ::traces :file-name string? :commits-hash string?)
        :ret (s/or :success string? :fail nil?))

(defn new-name-for
  "Returns current name of an entity that existed in revision represented by commit-hash"
  [{:keys [commits-order same-names-remap rename-graph current-names] :as traces} file-name commit-hash]
  (when-let [rev-pos (get commits-order commit-hash)]
    (let [revs (->> file-name
                    (get same-names-remap)
                    (take-while #(<= (get commits-order %) rev-pos)))
          last-rev (last revs)
          unique-name (if (and revs last-rev (> (count revs) 1))
                        (entity->unique-name file-name last-rev)
                        file-name)
          root (or (get rename-graph unique-name) file-name)]
      (log/debug "new-name-for:" rev-pos revs last-rev unique-name root)
      (get current-names root))))

(defn mine-single-git-renames!
  [output-dir index]
  (let [raw-data (paths/as-indexed-file-name output-dir "intermediate-results-for-renaming" index "csv")
        traces-file (paths/as-indexed-file-name output-dir "traces" index "edn")
        traces (build-traces-from-file raw-data)]
    (io/delete-file raw-data)
    traces))

(defn- exist-note-file?
  [note new-entity]
  (.exists (io/as-file (paths/as-child-path (:repo-path note) new-entity))))

(defn- find-new-entity
  [traces note]
  (let [new-entity (or (new-name-for traces (:last-entity note) (:last-rev note))
                       (new-name-for traces (:original-entity note) (:original-rev note)))]
    (if (and new-entity (exist-note-file? note new-entity))
      new-entity
      (if (exist-note-file? note (:last-entity note))
        (:last-entity note)
        nil))))

(defn- new-names-for-single-repo
  [new-notes old-notes output-dir index]
  (let [traces (mine-single-git-renames! output-dir index)]
    (into new-notes
          (map #(assoc % :new-entity (find-new-entity traces %)))
          old-notes)))

(defn new-names-for
  [repo-paths notes output-dir]
  (let [repo-paths-set (set repo-paths)
        notes-by-repo (group-by :repo-path notes)]
    (into
     (reduce
      (fn [new-notes [idx repo-path]]
        (log/trace "Performing entity renaming for <" repo-path ">")
        (new-names-for-single-repo new-notes (notes-by-repo repo-path) output-dir idx))
      [] (map-indexed vector repo-paths))
     (->> notes-by-repo
          keys
          (filter (comp not repo-paths-set))
          (mapcat notes-by-repo)))))

;;; The analysis part of notes does not currently expect protocols (ie
;;; file:path/etc)
(s/def ::original-entity ::note-specs/entity-does-not-have-protocol)
(s/def ::last-entity ::note-specs/entity-does-not-have-protocol)
(s/def ::new-entity ::note-specs/entity-does-not-have-protocol)

(s/def ::no-protocol-entity-in-notes (s/keys :req-un [::original-entity
                                              ::last-entity]))

(s/def ::note (s/merge ::note-specs/note-base ::no-protocol-entity-in-notes))

(s/def ::out-note-has-optional-new-entity (s/keys ::opt-on [::new-entity]))
(s/def ::out-note (s/merge ::note ::out-note-has-optional-new-entity))

(s/fdef new-names-for-notes
        :args (s/cat :repo-paths ::note-specs/repo-paths :notes ::note-specs/notes :output-dir ::note-specs/output-dir)
        :ret (s/keys :req-un [::note-specs/found ::note-specs/lost]))

(defn new-names-for-notes!
  [repo-paths notes output-dir]
  (let [new-notes (new-names-for repo-paths notes output-dir)
        separated-notes {:found (into []
                                      (comp (filter :new-entity)
                                            (map #(cond-> %
                                                    (= (:new-entity %) (:last-entity %))
                                                    (dissoc :new-entity))))
                                      new-notes)
                         :lost (into []
                                     (comp (filter (comp nil? :new-entity))
                                           (map #(dissoc % :new-entity)))
                                     new-notes)}
        new-notes-file-name (paths/as-child-path output-dir "notes-renames.json")
        lost-notes-file-name (paths/as-child-path output-dir "lost-notes.json")]
    (log/debug "Persisting notes file renames: " new-notes-file-name lost-notes-file-name)
    (with-open [out-file (io/writer new-notes-file-name)]
      (json/generate-stream (:found separated-notes) out-file))
    (with-open [out-file (io/writer lost-notes-file-name)]
      (json/generate-stream (:lost separated-notes) out-file))
    separated-notes))
