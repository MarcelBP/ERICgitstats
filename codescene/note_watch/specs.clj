(ns codescene.note-watch.specs
  (:require
   [codescene.analysis.specs :as specs]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]))

(s/def ::string string?)

;;; see codescene-cloud-web.specs for a version of this with a generator
(s/def ::non-empty-string (s/and ::string (complement string/blank?)))
(s/def ::id nat-int?)

;;; <begin specs from on-prem notes.shared-specs>


;;; Right now, an entity can only be a file, so files should look like
;;; file:/path/to/file. Later other kinds of entities will be added.
(def entity-protocols #{"file"})

(s/def ::entity-has-supported-protocol (s/and
                                         ::non-empty-string
                                         #(when-let [matches (re-matches #"^([^:]+):.+$" %)]
                                                 (entity-protocols (second matches)))))

(s/def ::entity-does-not-have-protocol (s/and
                                         ::non-empty-string
                                         #(let [matches (re-matches #"^([^:]+):.+$" %)]
                                            (or (not matches)
                                                (not (entity-protocols (second matches)))))))

;;; NOTES


(s/def ::note-text (s/nilable ::string))
(s/def ::category (s/nilable #{"supervise" "refactor" "no-problem"}))
(s/def ::project-root ::non-empty-string)

(s/def ::repo-path ::non-empty-string)


(defn- only-hex-chars? [s]
  (let [hex-chars (set "0123456789abcdef")]
    (every? hex-chars s)))

(s/def ::hex-chars only-hex-chars?)
(s/def ::rev (s/and ::non-empty-string ::hex-chars))
(s/def ::original-rev ::rev)
(s/def ::last-rev ::rev)

(s/def ::lost boolean?)

;;; RISK NOTES (notes contained inside a risk-note map)

(s/def ::biomarkers-warning (s/nilable #{"warning" "ok" "no-change" "not-calculated"}))
(s/def ::trends-warning (s/nilable #{"warning" "ok" "no-change"}))
(s/def ::warnings (s/nilable (s/keys :req-un [::biomarkers-warning ::trends-warning])))

(s/def ::score-integer nat-int?)
(s/def ::original ::score-integer)
(s/def ::last ::score-integer)
(s/def ::new ::score-integer)
(s/def ::score-map (s/keys :req-un [::original ::last ::new]))
(s/def ::biomarkers ::score-map)
(s/def ::trends ::score-map)
(s/def ::scores (s/keys :req-un [::biomarkers ::trends]))

;;; <end from on-prem notes.shared-specs >


;;; <begin specs from note-watch.renames >


(s/def ::output-dir string?)
(s/def ::entity string?)
(s/def ::new-entity string?)
(s/def ::rev string?)
(s/def ::note-text string?)
(s/def ::category string?)
(s/def ::created ::specs/date-str)

(s/def ::repo-paths (s/* ::repo-path))
(s/def ::notes (s/* ::note))
(s/def ::out-notes (s/* ::out-note))
(s/def ::found ::out-notes)
(s/def ::lost ::out-notes)


;;; <end specs from note-watch.renames >


(s/def ::note-base (s/keys :req-un [::repo-path
                                    ::last-rev
                                    ::original-rev
                                    ::category
                                    ::note-text
                                    ::creation-date]))
