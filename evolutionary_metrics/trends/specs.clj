(ns evolutionary-metrics.trends.specs
  (:require [clojure.spec.alpha :as s]
            [evolutionary-metrics.core :as core]))


;;; author contribution

(s/def ::date-now ::core/datetime)
(s/def ::time-in-months nat-int?)
(s/def ::time-threshold
  (s/keys :req-un [::date-now ::time-in-months]))

(comment

  (def ^:private author-stats
    ; :author :added :deleted :net :revisions :months :lastcontrib
    [["Oldtimer"   0 0 0 0 23 "2015-11-11"]
     ["Consultant" 0 0 0 0 10 "2015-01-01"]
     ["Exdev"      0 0 0 0  2 "2015-02-01"]])
  )
(def year-month-date-regex #"\d{4}-\d{2}-\d{2}")
(s/def ::change-value (s/or :int nat-int? :int-str (s/and string? #(Integer/parseInt %))))
(s/def ::net-change-value (s/or :int int? :int-str (s/and string? #(Integer/parseInt %))))
(s/def ::author-contrib-date (s/and string? #(re-matches year-month-date-regex %)))
(s/def ::author-stat
  (s/tuple string? ::change-value ::change-value ::net-change-value ::change-value ::change-value
           ::author-contrib-date ::author-contrib-date))
(s/def ::author-stats (s/coll-of ::author-stat))

;;; rolling average

(s/def ::rolling-avg-days nat-int?)

(s/def ::churn-column #{:added :deleted :modified})
(s/def ::target-column #{:rollingadded :rollingdeleted :rollingmodified})

;;; complexity trend step detection

(s/def ::trend-period-in-months nat-int?)
(s/def ::trend-warning-look-back-in-months nat-int?)
(s/def ::min-sample-points nat-int?)
(s/def ::min-loc nat-int?)
(s/def ::threshold-in-sd-yellow nat-int?)
(s/def ::threshold-in-sd-red nat-int?)
(s/def ::min-percentage-yellow float?)
(s/def ::min-percentage-red float?)
(s/def ::complexity-thresholds (s/keys :req-un [::trend-period-in-months
                                                ::trend-warning-look-back-in-months
                                                ::min-sample-points
                                                ::min-loc
                                                ::threshold-in-sd-yellow
                                                ::threshold-in-sd-red
                                                ::min-percentage-yellow
                                                ::min-percentage-red]))

(s/def ::hotspot-name string?)
(s/def ::hotspot-revisions nat-int?)
(s/def ::complexity-trend-dataset-fn ifn?) ; we want to get rid of the details on how the complexity trend is persisted
(s/def ::hotspot-trend-file (s/keys :req-un [::hotspot-name
                                             ::hotspot-revisions
                                             ::complexity-trend-dataset-fn]))
(s/def ::hotspot-trend-files (s/coll-of ::hotspot-trend-file))

(s/def ::yellow-warning nat-int?)
(s/def ::red-warning nat-int?)
(s/def ::complexity-trend-classification (s/tuple ::yellow-warning ::red-warning))
(s/def ::hotspot-with-complexity-trend-warning (s/keys :req-un [::hotspot-name ::complexity-trend-classification]))
(s/def ::hotspots-with-complexity-trend-warnings (s/coll-of ::hotspot-with-complexity-trend-warning))

