(ns evolutionary-metrics.complexity.specs
  (:require [clojure.spec.alpha :as s]
            [evolutionary-metrics.core :as core]))

(s/def ::parsed-entity-namespace string?)
(s/def ::ignored-files-fn fn?)
(s/def ::keep-specific-file-fn ifn?) ; typically used to filter away files that aren't under version-control
(s/def ::exclude-files ::core/pattern)

(s/def ::auto-detect-text-files boolean?)

(s/def ::detailed-stats-options (s/keys :req-un [::parsed-entity-namespace
                                                 ::ignored-files-fn
                                                 ::keep-specific-file-fn
                                                 ::exclude-files
                                                 ::auto-detect-text-files]))
(s/def ::summary-options (s/keys :req-un [::parsed-entity-namespace
                                          ::keep-specific-file-fn
                                          ::ignored-files-fn
                                          ::auto-detect-text-files]))
(s/def ::summaries (s/coll-of ::core/filename))
(s/def ::language string?)
(s/def ::number-str (s/and string? #(Integer/parseInt %)))
(s/def ::scrambled-content (s/coll-of (s/tuple ::language ::core/filename ::number-str ::number-str ::number-str)))