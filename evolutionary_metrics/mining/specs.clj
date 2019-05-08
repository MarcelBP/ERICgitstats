(ns evolutionary-metrics.mining.specs
  (:require [clojure.spec.alpha :as s]
            [evolutionary-metrics.core :as core]
            [clojure.java.io :as io]))

;; Git access

(s/def ::git-exe-command string?)

; Support both abbreviated and full commit hashes
(s/def ::git-commit-hash (s/and string? #(re-matches #"[0-9a-f]{7,40}" %)))

(s/def ::git-head (s/and string? #(= "HEAD" %)))

;; Repository mining

(defn- path-exists?
  [path]
  (some-> path io/as-file .exists))

(s/def ::repository-path (s/and string?
                                path-exists?))

;; Alias the specs since we don't want to break backwards compatibility now.
(s/def ::rev (s/or
               :latest-revision ::git-head
               :specific-revision ::git-commit-hash))

(s/def ::git-revisions (s/coll-of ::rev))

(s/def ::name ::core/filename)

(s/def ::filenames (s/coll-of ::core/filename))

(s/def ::file-in-revision (s/and (s/keys :req-un [::rev ::name])))

(s/def ::file-content string?)

(s/def ::git-error-message string?)

(s/def ::mined-git-log (s/coll-of string?)) ;; we could make this much more specific...
