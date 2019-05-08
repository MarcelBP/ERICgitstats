;;; Copyright (C) 2015 Empear

(ns evolutionary-metrics.core
  (:require [clj-time.core :as t]
            [clj-time.format :as tf]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen])
  (:import (org.joda.time DateTime)
           (java.io File)))

(s/def ::file #(instance? File %))
(s/def ::filename (s/and string?))

(s/def ::datetime (s/with-gen #(instance? DateTime %)
                    #(gen/fmap (partial apply t/date-time)
                               (gen/tuple (gen/choose 1990 2050)
                                          (gen/choose 1 12)
                                          ;; avoid trouble with 29, 30, 31 Feb, etc
                                          (gen/choose 1 28)
                                          (gen/choose 0 23)
                                          (gen/choose 0 59)
                                          (gen/choose 0 59)
                                          (gen/choose 0 999)))))

(s/def ::date-string (s/with-gen (s/and string? #(re-matches #"\d{4}-\d{2}-\d{2}" %))
                       #(gen/fmap (fn [dt] (tf/unparse (:year-month-day tf/formatters) dt)) (s/gen ::datetime))))

(s/def ::basic-date-time-string (s/with-gen (s/and string? #(re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(((\+|-)\d+:\d+)|Z)" %))
                                            #(gen/fmap (fn [dt] (tf/unparse (:date-time-no-ms tf/formatters) dt)) (s/gen ::datetime))))

(s/def ::pattern string?)

(def gen-hex-char
  (gen/elements (concat (map char (range (int \a) (int \f)))
                        (range 0 9))))

(defn gen-hex-string [len]
  (gen/fmap clojure.string/join (gen/vector gen-hex-char len len)))

(s/def ::rev (s/with-gen
               (s/and string?
                      #(re-matches #"[a-zA-Z0-9]{1,40}" %))
               #(gen-hex-string 7)))

(s/def ::ticket-id string?) ; could be anything, not just a number

(s/def ::author-name string?)
(s/def ::author-email string?)
(s/def ::entity string?)

(s/def ::existing-path (s/and string? #(.exists (io/as-file %))))

;; Specify the options used to control grouping, etc.
;;

(s/def ::regular-expression string?)

(s/def ::version-control #{"git" "id"})

(s/def ::temporal-by-ticket-id 1)
(s/def ::message-extraction-pattern ::regular-expression)

(s/def ::options (s/keys :req-un [::version-control]
                         :opt-un [::temporal-by-ticket-id
                                  ::message-extraction-pattern]))

(s/def ::analysis-result-header (s/coll-of (s/or :header keyword? :name string?)))
(s/def ::analysis-result-values (s/coll-of any?))
(s/def ::analysis-result (s/keys :req-un [::analysis-result-header ::analysis-result-values]))