(ns evolutionary-metrics.parsers
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [evolutionary-metrics.core :as core]))

(s/def ::entity-diff (s/with-gen (s/and string? #(re-matches #"(-|\d+)" %))
                       #(gen/fmap (fn [x] (str x))
                                  (gen/frequency [[2 (gen/int)] [1 (gen/return "-")]]))))

(s/def ::author ::core/author-name)
(s/def ::rev ::core/rev)
(s/def ::date ::core/date-string)
(s/def ::entity ::core/filename)
(s/def ::message string?)
(s/def ::loc-added ::entity-diff)
(s/def ::loc-deleted ::entity-diff)
(s/def ::author-email ::core/author-email)
(s/def ::basic-date-time ::core/basic-date-time-string)


(s/def ::vcs-commit (s/keys :req-un [::author
                                     ::rev
                                     ::date
                                     ::entity
                                     ::message
                                     ::loc-added
                                     ::loc-deleted
                                     ::author-email
                                     ::basic-date-time]))

(s/def ::vcs-log (s/coll-of ::vcs-commit))
