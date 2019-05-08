(ns codescene.reports.pdf-reports-spec
  (:require [clojure.spec.alpha :as s]))

(s/def ::size-metric nat-int?)
(s/def ::float-metric (s/or :none zero? :ratio ratio? :float float? :divisible nat-int?))

(s/def ::nilable-int (s/nilable nat-int?))
(s/def ::nilable-float (s/nilable ::float-metric))
(s/def ::nilable-string (s/nilable string?))

(s/def ::abandoned-code ::nilable-float)
(s/def ::active-authors ::nilable-int)
(s/def ::analysis-time any?)
(s/def ::former-developers ::nilable-int)
(s/def ::code-health ::nilable-int)
(s/def ::commits-last-month ::nilable-int)
(s/def ::last-month-code-health ::code-health)
(s/def ::last-year-code-health ::code-health)
(s/def ::lines-of-code ::nilable-string)
(s/def ::missed-goals ::nilable-int)
(s/def ::total-goals ::nilable-int)
(s/def ::title string?)
(s/def ::description string?)
(s/def ::branch-name string?)
(s/def ::lead-time ::nilable-int)
(s/def ::risk ::nilable-int)
(s/def ::nbr-of-commits ::nilable-int)
(s/def ::date string?)
(s/def ::commits-per-week nat-int?)
(s/def ::authors-per-week nat-int?)
(s/def ::authors-per-month nat-int?)
(s/def ::sub-system string?)
(s/def ::team-autonomy keyword?)
(s/def ::system-mastery ::nilable-float)
(s/def ::defects ::nilable-int)
(s/def ::last-defects ::nilable-int)
(s/def ::severity ::nilable-int)
(s/def ::file string?)
(s/def ::comment string?)
(s/def ::timespan keyword?)
(s/def ::title string?)
(s/def ::date string?)
(s/def ::font string?)
(s/def ::img-dir string?)
(s/def ::delivery-risk ::nilable-int)

(s/def ::system-health (s/keys :req-un [::abandoned-code
                                        ::active-authors
                                        ::analysis-time
                                        ::code-health
                                        ::commits-last-month
                                        ::delivery-risk
                                        ::former-developers
                                        ::last-month-code-health
                                        ::last-year-code-health
                                        ::lines-of-code
                                        ::missed-goals
                                        ::total-goals]))

(s/def ::early-warning-details-item string?)
(s/def ::early-warning-details (s/coll-of ::early-warning-details-item))
(s/def ::early-warnings-item (s/keys :req-un [::title
                                              ::description
                                              ::early-warning-details]))
(s/def ::early-warnings (s/coll-of ::early-warnings-item))

(s/def ::delivery-risks-item (s/keys :req-un [::branch-name
                                              ::lead-time
                                              ::risk
                                              ::nbr-of-commits]))
(s/def ::delivery-risks (s/coll-of ::delivery-risks-item))

(s/def ::goal-details string?)
(s/def ::goal-item (s/keys :req-un [::file
                                    ::goal-details]))
(s/def ::missed-refactorings (s/coll-of ::goal-item))
(s/def ::missed-supervisions (s/coll-of ::goal-item))
(s/def ::missed-no-problems (s/coll-of ::goal-item))

(s/def ::goals (s/keys :req-un [::missed-goals
                                ::total-goals
                                ::missed-refactorings
                                ::missed-supervisions
                                ::missed-no-problems]))

(s/def ::development-output-over-time-item (s/keys :req-un [::date
                                                            ::commits-per-week
                                                            ::authors-per-week]))
(s/def ::development-output-over-time (s/coll-of ::development-output-over-time-item))
(s/def ::active-authors-over-time-item (s/keys :req-un [::date
                                                        ::authors-per-month]))
(s/def ::active-authors-over-time (s/coll-of ::active-authors-over-time-item))
(s/def ::system-trends (s/keys :req-un [::development-output-over-time
                                        ::active-authors-over-time]))

(s/def ::subsystem-health-item (s/keys :req-un [::sub-system
                                                ::team-autonomy
                                                ::system-mastery
                                                ::code-health
                                                ::last-month-code-health
                                                ::last-year-code-health
                                                ::defects
                                                ::last-defects
                                                ::delivery-risk]))
(s/def ::subsystem-health (s/coll-of ::subsystem-health-item))

(s/def ::hotspot-details-item (s/keys :req-un [::severity
                                               ::description]))
(s/def ::hotspot-details (s/coll-of ::hotspot-details-item))
(s/def ::hotspots-item (s/keys :req-un [::file
                                        ::code-health
                                        ::last-month-code-health
                                        ::last-year-code-health
                                        ::hotspot-details
                                        ::comment]))
(s/def ::system-hotspots (s/coll-of ::hotspots-item))
(s/def ::hotspots-by-subsystem (s/map-of string? (s/coll-of ::hotspots-item)))

(s/def ::report-data (s/keys :opt-un [::system-health
                                      ::early-warnings
                                      ::delivery-risks
                                      ::goals
                                      ::system-trends
                                      ::subsystem-health
                                      ::system-hotspots
                                      ::hotspots-by-subsystem]))
(s/def ::report-datas-by-project (s/map-of string? ::report-data))

(s/def ::report-options (s/keys :opt-un [::timespan
                                         ::title
                                         ::date
                                         ::font
                                         ::img-dir]))