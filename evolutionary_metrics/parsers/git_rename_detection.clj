(ns evolutionary-metrics.parsers.git-rename-detection
  (:require [clojure.string]
            [clojure.spec.alpha :as s]))

(defn- trim-combined-path-separators
  "We may get a double slash when we combine parts of a
   path name in our re-name detection. Instead of preventing it,
   just do it and filer them here afterwards."
  [file-name]
  (clojure.string/replace file-name #"//" "/"))

;; A partial rename is when we modify some sub-path, e.g. resources/public/css/imgs/{ => dashboard}/hotspotsbackground.png
(def ^:private partial-rename-pattern #"^(.*)\{(.*) => (.*)\}(.*)$")

(defn- resolve-partial-rename
  [[_ prelude old-path new-path postlude]]
  {:pre [(some? old-path) (some? new-path)]}
  {:old-name (trim-combined-path-separators (str prelude old-path postlude))
   :new-name (trim-combined-path-separators (str prelude new-path postlude))})

;; A complete rename is when we move the content to a
;; new base path, e.g. src/create_tables.sql => resources/migrations/0001-initial.up.sql
(def ^:private complete-rename-pattern #"^(.*) => (.*)$")

(defn- resolve-complete-rename
  [[_ old-path new-path]]
  {:pre [(some? old-path) (some? new-path)]}
  {:old-name old-path
   :new-name new-path})

(defn renamed?
  [file-name]
  (if-let [partial-rename (re-matches partial-rename-pattern file-name)]
    (resolve-partial-rename partial-rename)
    (if-let [complete-rename (re-matches complete-rename-pattern file-name)]
      (resolve-complete-rename complete-rename))))

(defn- resolve-new-name-from
  [rename-cache file-name]
  (loop [cache rename-cache
         current-name file-name
         renames #{}]
    (let [renamed-file-name? (get (:renames cache) current-name)]
      (if (or (not renamed-file-name?)
              (get renames renamed-file-name?)) ; break rename cycles
        {:name current-name :rename-cache cache}
        (recur cache renamed-file-name? (conj renames renamed-file-name?)))))) ; check for multiple levels of renames while avoiding cycles

(defn- reverse-to-latest-name?
  "Some file renames introduce cycles. In case we already have a record of a
   rename we ignore sub-sequent attempts to rename again.
   For example:
                rev 2: a => b
                rev 1: b => a. ; we already have b as present name -> skipe the rename"
  [{:keys [reverse-rules]} old-name new-name]
  (if-let [known-rule (get reverse-rules old-name)]
    (= known-rule new-name)))

(defn- extend-cache
  [{:keys [renames reverse-rules] :as cache} {:keys [old-name new-name]}]
  (if (reverse-to-latest-name? cache old-name new-name)
    cache
    {:renames (merge renames {old-name new-name})
     :reverse-rules (merge reverse-rules {new-name old-name})}))

(defn- cached-rename-of
  [{:keys [new-name] :as rename-rule} rename-cache]
  (let [{:keys [name]} (resolve-new-name-from rename-cache new-name)]
    {:name name
     :rename-cache (extend-cache rename-cache rename-rule)}))

(defn file-extracted-from-existing-file?
  [old-name-in-commit? file-name]
  (if-let [renamed-parts (renamed? file-name)]
    {:extracted? (some? (old-name-in-commit? (:old-name renamed-parts)))
     :extraction-name (:new-name renamed-parts)}
    {:extracted? false
     :extraction-name ""}))

(defn- introduce-as-new-file
  [{:keys [new-name] :as _rename-rule} rename-cache]
  (resolve-new-name-from rename-cache new-name))

(def ^:const empty-rename-cache {:renames {} :reverse-rules {}})

(s/def ::name string?)
(s/def ::old-name ::name)
(s/def ::new-name ::name)
(s/def ::renames (s/map-of ::old-name ::new-name))
(s/def ::reverse-rules (s/map-of ::new-name ::old-name))
(s/def ::rename-cache (s/keys :req-un [::renames
                                       ::reverse-rules]))

(s/def ::adjusted-name-result (s/keys :req-un [::name
                                               ::rename-cache]))


(s/fdef adjusted-name-for
        :args (s/cat :rename-cache ::rename-cache
                     :extracted? boolean?
                     :entity string?)
        :ret ::adjusted-name-result)

(defn adjusted-name-for
  "Resolves a new file name, in case the content has been renamed, by
   use of a cache with re-names.
   We need to look ahead in file-name-in-commit? because if we split a file
   in two, Git will mark the new file as a rename of the old. So splitting content
   is a special case that we do NOT treat as a rename.
   Note that the algorithm requires the log to be parsed from latest to oldest."
  [rename-cache extracted? entity]
  (if-let [renamed-parts (renamed? entity)]
    (if extracted?
      (introduce-as-new-file renamed-parts rename-cache)
      (cached-rename-of renamed-parts rename-cache))
    (resolve-new-name-from rename-cache entity)))

