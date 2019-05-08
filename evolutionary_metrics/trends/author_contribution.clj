(ns evolutionary-metrics.trends.author-contribution
  (:require [clj-time.core :as tc]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [evolutionary-metrics.parsers.time-parser :as tp]
            [evolutionary-metrics.core :as core]
            [evolutionary-metrics.analysis.internal.authors-shared :as authors-shared]
            [evolutionary-metrics.trends.specs :as trend-specs]))

(defn- remove-known-ex-devs
  [author-stats ex-devs]
  (let [lookupable-ex-devs (set ex-devs)]
    (remove (fn [[name]] (lookupable-ex-devs name)) author-stats)))

(def time-parser (tp/time-parser-from "yyyy-MM-dd"))

(defn- as-time
  [time-as-string]
  (time-parser time-as-string))

(defn- cut-off-time
  [{:keys [date-now time-in-months]}]
  (-> date-now
      (tc/minus (tc/months time-in-months))))

(s/fdef authors-without-contribution
        :args (s/cat :time-threshold ::trend-specs/time-threshold
                     :author-stats ::trend-specs/author-stats
                     :known-ex-devs (s/coll-of string?))
        :ret (s/coll-of (s/tuple string? string?)))

(defn authors-without-contribution
  [time-threshold author-stats known-ex-devs]
  (let [sans-ex-devs (remove-known-ex-devs author-stats known-ex-devs)
        cut-off (cut-off-time time-threshold)]
    (->> sans-ex-devs
         (filter (fn [a] (tc/before? (as-time (authors-shared/last-contribution-for a)) cut-off)))
         (map (fn [a] [(first a) (authors-shared/last-contribution-for a)]))))) ; NASTY (again), we depend upon a particular order!

(s/fdef detect-authors-without-contribution
        :args (s/alt :std-out (s/cat :time-threshold ::trend-specs/time-threshold
                                     :author-churn-file-name ::core/filename
                                     :known-ex-devs (s/coll-of string?))
                     :out-file (s/cat :time-threshold ::trend-specs/time-threshold
                                      :author-churn-file-name ::core/filename
                                      :known-ex-devs (s/coll-of string?)
                                      :output-file-name ::core/filename))
        :ret (s/coll-of (s/tuple string? string?)))

(defn detect-authors-without-contribution
  ([time-threshold author-churn-file-name known-ex-devs]
   (let [author-stats (shared/read-csv-sans-header-from author-churn-file-name)
         non-contributing (authors-without-contribution time-threshold author-stats known-ex-devs)]
     (csv/write-csv *out* [["author" "lastcontrib"]])
     (csv/write-csv *out* non-contributing)
     non-contributing))
  ([time-threshold author-churn-file-name known-ex-devs output-file-name]
   (with-open [out-file (io/writer output-file-name)]
     (binding [*out* out-file]
       (detect-authors-without-contribution time-threshold author-churn-file-name known-ex-devs)))))
