;;; Copyright (C) 2015 Adam Tornhill
;;;

(ns evolutionary-metrics.trends.complexity-trend
  (:require [evolutionary-metrics.mining.ws-complexity :as wsc]
            [evolutionary-metrics.complexity.loco :as loco]
            [evolutionary-metrics.mining.vcs :as vcs]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

;;; A complexity trend is a natural way to x-ray hotspots; Without a trend
;;; we cannot tell if the hotspot glows because we're refactoring it, or,
;;; the usual case, because we're contributing to a grand decline.

;;; Limitations: only works on Git right now, but the majority of the
;;; code is vcs-independent. I'd suggest that we introduce a protocol
;;; for our vcs and parameterize this trend analysis with that.

;;; Hardcode the definition of a logical unit of indent for now.
;;; We could parameterize with this, but since we're after trends it
;;; doesn't really matter...I think.
(def ^:constant options
  {:tabs 1
   :spaces 4})

(defn trend
  [historic-versions language-dependent-rules]
  (map (fn [{:keys [date content rev]}]
         (let [{:keys [total n mean median sd max comments complexity-loc-ratio]}
               (wsc/total-indent-complexity options
                                            content
                                            language-dependent-rules)]
           [date total n mean median sd max comments complexity-loc-ratio rev]))
       historic-versions))

(defn- keep-valid
  "Drop the revisions where we, for some reason - not expected, failed to
   obtain the historic version.
   NOTE: we should probably log or report this problem in case it happens."
  [all-historic-with-status]
  (->> all-historic-with-status
       (filter first)
       (map second)))

(defn language-rules-for
  [file-name]
  (or (-> file-name
          (clojure.string/split #"/")
          last
          loco/rule-for-file-name)
      (loco/fallback-text-rule file-name)))

(def ^:private n-high-fidelity-sample-points 5)

(defn revisions-for-trend
  "We don't want to include every revision because it eats performance and
   provides little, if any, value.
   Instead we use an algorithm that always picks the last n revisions and
   10% of the rest. Those 10% are spread out evenly. We also ensure we get
   the first revision to provide a starting point."
  [vs]
  (let [oldest (first vs)
        other-commits (->> vs (drop 1) reverse)
        [recent-details old-details] (split-at n-high-fidelity-sample-points other-commits)
        historic-sample-rate (Math/round (Math/ceil (* (count old-details) 0.05)))
        historic-samples (->> old-details (take-nth historic-sample-rate) (remove (partial = oldest)))]
    (reverse (concat recent-details historic-samples [oldest]))))

(defn- historic-versions-with-metadata
  [commit-exclusion-lookup git-cmd repo start-date file-name]
  (let [exclude-commit? (partial commit-exclusion-lookup repo)]
    (->> (vcs/historic-file-versions-metadata git-cmd repo start-date file-name)
         (remove (comp exclude-commit? :rev))
         revisions-for-trend
         (map (partial vcs/lazy-historic-file-version git-cmd repo))
         keep-valid)))

(defn extend-with-possible-deletion-point
  [complexity-rows content-deletion-lookup]
  (if-let [deletion-date (content-deletion-lookup)]
    (conj (vec complexity-rows) [(:date deletion-date) 0.0 0 0.0 0.0 0.0 0 0 0.0 (:rev deletion-date)])
    complexity-rows))

(defn calculate-trend
  [commit-exclusion-lookup git-cmd content-deletion-lookup repo start-date file-name out-file-name]
  (let [historic-versions (historic-versions-with-metadata commit-exclusion-lookup git-cmd repo start-date file-name)
        language-dependent-rules (language-rules-for file-name)
        complexity-trend (trend historic-versions language-dependent-rules)
        deletion-aware-trend (extend-with-possible-deletion-point complexity-trend content-deletion-lookup)]
    (with-open [out-file (io/writer out-file-name)]
       (csv/write-csv out-file [["date" "total" "n" "mean" "median" "sd" "max" "comments" "ratio" "revision"]])
       (csv/write-csv out-file deletion-aware-trend))))
