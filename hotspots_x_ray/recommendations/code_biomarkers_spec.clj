(ns hotspots-x-ray.recommendations.code-biomarkers-spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::size-metric nat-int?)
(s/def ::float-metric (s/or :none zero? :ratio ratio? :float float? :divisible nat-int?))

(s/def ::active-code-size ::size-metric)
(s/def ::lines-in-file ::size-metric)
(s/def ::longest-fn-loc ::size-metric)
(s/def ::longest-fn-loc-name string?)
(s/def ::n-functions ::size-metric)

(s/def ::n-clones ::size-metric)
(s/def ::clone-ratio ::float-metric)

(s/def ::cc-max nat-int?)
(s/def ::cc-max-name string?)
(s/def ::cc-mean ::float-metric)
(s/def ::cc-total ::size-metric)
(s/def ::median-fn-loc ::float-metric)

;; Optional function argument statistics
(s/def ::fn-args (s/keys :req-un [::max-args
                                  ::mean-args
                                  ::n-args
                                  ::n-primitives
                                  ::n-string-args]))

(s/def ::max-nested-complexity-depth nat-int?)
(s/def ::max-nested-complexity-depth-name string?)
(s/def ::max-nested-in-global-scope nat-int?)

(s/def ::main-body-cc nat-int?)

(s/def ::nested (s/keys :req-un [::max-nested-complexity-depth
                                 ::max-nested-complexity-depth-name
                                 ::nested-complexity-of-interest]
                        :opt-un [::max-nested-in-global-scope]))

(s/def ::owner string?)
(s/def ::ownership ::float-metric)
(s/def ::knowledge-loss ::float-metric)

(s/def ::social (s/keys :req-un [::owner
                                 ::ownership
                                 ::knowledge-loss]))

(s/def ::code-properties (s/keys :req-un [::active-code-size
                                          ::longest-fn-loc
                                          ::longest-fn-loc-name
                                          ::median-fn-loc
                                          ::n-functions

                                          ::nested

                                          ::n-clones
                                          ::clone-ratio

                                          ::cc-max
                                          ::cc-max-name
                                          ::cc-mean
                                          ::cc-total]
                                 ; Optional for backwards compatibility with existing, published analysis results; the
                                 ; indicators are calculated based on stored historic values and these fields might
                                 ; not be present there if the analysis was run on an old version of CodeScene.
                                 :opt-un [::fn-args
                                          ::lines-in-file
                                          ::main-body-cc
                                          ::social]))

(s/def ::ths map?)
(s/def ::lcom4-value nat-int?)
(s/def ::code-score nat-int?)

