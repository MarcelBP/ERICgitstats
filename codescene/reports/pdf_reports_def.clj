(ns codescene.reports.pdf-reports-def)

(def ^:const page-width 595)
(def ^:const page-height 842)
(def ^:const system-mastery-high 0.7)
(def ^:const system-mastery-low 0.3)
(def ^:const code-health-high 8)
(def ^:const code-health-low 3)
(def ^:const delivery-risk-high 9)
(def ^:const delivery-risk-low 7)
(def ^:const code-health-trend-high 5)
(def ^:const code-health-trend-low 2)

(def lightest-grey [247 246 245])
(def light-grey [216 216 216])
(def medium-grey [142 142 142])
(def green [13 112 93])
(def green-transparent [195 213 209])
(def red [254 104 73])
(def red-transparent [255 200 180])
(def yellow [250 218 50])
(def yellow-transparent [255 255 195])
(def font-blue [55 84 145])
(def font-blue [55 84 145])
(def black [0 0 0])
(def white [255 255 255])

(def stylesheet
  {
   ;; Generic text styles
   :title                                          {:size 36 :align :center :spacing-after 60}
   :subtitle                                       {:size 24 :align :center :color [128 128 128]}
   :heading                                        {:style "bold" :size 18 :color font-blue}
   :heading2                                       {:style "bold" :size 14 :color font-blue}
   :body                                           {:size 11}

   ;; Value dependant cell styles
   :team-autonomy-unset-cell                       {:background-color light-grey}
   :team-autonomy-neutral-cell                     {:background-color light-grey}
   :team-autonomy-ok-cell                          {:background-color green-transparent}
   :team-autonomy-alarm-cell                       {:background-color red-transparent}

   :system-mastery-unset-cell                      {:background-color light-grey}
   :system-mastery-ok-cell                         {:background-color green-transparent}
   :system-mastery-warning-cell                    {:background-color yellow-transparent}
   :system-mastery-alarm-cell                      {:background-color red-transparent}

   :code-health-unset-cell                         {:background-color light-grey}
   :code-health-ok-cell                            {:background-color green}
   :code-health-warning-cell                       {:background-color yellow}
   :code-health-alarm-cell                         {:background-color red}

   :delivery-risk-unset-cell                       {:background-color light-grey}
   :delivery-risk-ok-cell                          {:background-color green-transparent}
   :delivery-risk-warning-cell                     {:background-color yellow-transparent}
   :delivery-risk-alarm-cell                       {:background-color red-transparent}

   :missed-goals-unset-cell                        {:background-color light-grey}
   :missed-goals-ok-cell                           {:background-color green-transparent}
   :missed-goals-alarm-cell                        {:background-color red-transparent}

   :defects-unset-cell                             {:background-color light-grey}
   :defects-neutral-cell                           {:background-color light-grey}

   ;; Value dependant text styles
   :hotspot-severity-ok                            {:color green}
   :hotspot-severity-warning                       {:color yellow}
   :hotspot-severity-alarm                         {:color red}

   ;; Hotspots table styles
   :hotspots-table                                 {:width-percent 100 :spacing-before 20}
   :hotspots-table-header-cell                     {:background-color lightest-grey}
   :hotspot-name-cell                              {}
   :hotspot-code-health-cell                       {:align :center :valign :middle}
   :hotspot-last-code-health-cell                  {:align :center :valign :middle}
   :hotspot-details-cell                           {}
   :hotspot-comment-cell                           {}
   :hotspots-table-header                          {:style "bold" :size 10 :color black}
   :hotspot-file-name                              {:size 9 :color black :spacing-before 0 :spacing-after 0}
   :hotspot-dir-name                               {:size 5 :color medium-grey :spacing-before 0 :spacing-after 0}
   :hotspot-code-health                            {:size 12 :leading 6 :color black}
   :hotspot-last-code-health                       {:size 12 :leading 6 :color black}
   :hotspot-details                                {:size 5 :color black :spacing-before 0 :spacing-after 0}
   :hotspot-comment                                {:size 5 :color black}

   ;; Missed goals table styles
   :missed-goals-table                             {:width-percent 100 :spacing-before 20}
   :missed-goals-table-header-cell                 {:background-color lightest-grey}
   :missed-goal-image-cell                         {}
   :missed-goal-name-cell                          {}
   :missed-goal-details-cell                       {}
   :missed-goals-table-header                      {:style "bold" :size 11}
   :missed-goal-file-name                          {:size 9 :color black :spacing-before 0 :spacing-after 0}
   :missed-goal-dir-name                           {:size 5 :color medium-grey :spacing-before 0 :spacing-after 0}
   :missed-goal-details                            {:size 5 :color black :spacing-before 0 :spacing-after 0}

   ;; Sub-system health table styles
   :sub-system-health-table                        {:background-color lightest-grey :width-percent 100 :spacing-before 20 :cell-border false :padding [0 0 0 0] :valign :center}
   :sub-system-spacing-cell                        {:background-color white :height 10}
   :sub-system-health-header-cell                  {:background-color white}
   :sub-system-name-cell                           {:align :left :valign :middle}
   :sub-system-team-autonomy-cell                  {:align :center :valign :middle}
   :sub-system-mastery-cell                        {:align :center :valign :middle}
   :sub-system-code-health-cell                    {:align :center :valign :middle :height 45}
   :sub-system-delivery-risk-cell                  {:align :center :valign :middle}
   :sub-system-defects-cell                        {:align :center :valign :middle}
   :sub-system-health-header                       {:size 10 :spacing-after 10 :style "bold"}
   :sub-system-name                                {:size 12 :leading 10 :color black}
   :sub-system-team-autonomy                       {:size 15 :leading 7 :color black}
   :sub-system-mastery                             {:size 15 :leading 7 :color black}
   :sub-system-code-health                         {:size 18 :leading 9 :color white}
   :sub-system-delivery-risk                       {:size 10 :leading 5 :color black}
   :sub-system-defects                             {:size 15 :leading 7 :color black}

   ;; Project summary table styles
   :project-table                                  {:background-color lightest-grey :width-percent 100 :spacing-before 20 :cell-border false :padding [2 2 2 2]}
   :project-description-and-status-table           {}
   :project-data-table                             {}
   :project-health-table                           {}
   :project-main-health-table                      {}
   :project-activity-image                         {:width 10}
   :project-name-cell                              {}
   :project-commits-last-month-cell                {}
   :project-analysis-time-cell                     {}
   :project-lines-of-code-cell                     {}
   :project-active-developers-cell                 {}
   :project-system-mastery-cell                    {:align :center :valign :middle :height 30}
   :project-delivery-risk-cell                     {:align :center :valign :middle :height 30}
   :project-missed-goals-cell                      {:align :center :valign :middle :height 30}
   :project-code-health-cell                       {:align :center :valign :middle :height 45}
   :project-name                                   {:size 11 :leading 11 :spacing-after 10 :color black}
   :project-commits-last-month                     {:size 7 :leading 7 :spacing-after 5 :color medium-grey}
   :project-analysis-time                          {:size 7 :leading 7 :spacing-after 3 :color black}
   :project-lines-of-code                          {:size 7 :leading 7 :spacing-after 3 :color black}
   :project-active-developers                      {:size 7 :leading 7 :spacing-after 3 :color black}
   :project-system-mastery                         {:size 10 :leading 5 :color black}
   :project-delivery-risk                          {:size 10 :leading 5 :color black}
   :project-missed-goals                           {:size 10 :leading 5 :color black}
   :project-total-goals                            {:size 7 :color black}
   :project-code-health                            {:size 20 :leading 10 :color white}
   :project-table-label                            {:size 7 :leading 7 :spacing-after 3 :color medium-grey}

   ;; System health table styles
   :system-health-table                            {:background-color lightest-grey :width-percent 100 :spacing-before 20 :cell-border false :padding [2 2 2 2]}
   :system-health-key-personnel-table              {}
   :system-health-code-health-table                {}
   :system-health-descriptive-data-table           {}
   :system-health-table-header-cell                {:align :center :valign :middle}
   :system-health-table-label-cell                 {:align :center :valign :middle}
   :system-health-key-personnel-image-cell         {:height 45}
   :system-health-key-personnel-cell               {:align :center :valign :middle}
   :system-health-abandoned-code-image-cell        {:height 45}
   :system-health-abandoned-code-cell              {:align :center :valign :middle}
   :system-health-code-health-table-cell           {:padding [0 40 0 40]}
   :system-health-code-health-cell                 {:align :center :valign :middle :height 45 :padding [0 0 0 0]}
   :system-health-last-code-health-table-cell      {:padding [0 55 0 55]}
   :system-health-last-code-health-cell            {:align :center :valign :middle :height 30 :padding [0 0 0 0]}
   :system-health-last-year-code-health-table-cell {:padding [0 55 0 55]}
   :system-health-last-year-code-health-cell       {:align :center :valign :middle :height 30 :padding [0 0 0 0]}
   :system-health-hotspot-logo-cell                {:height 50}
   :system-health-red-hotspot-ratio-cell           {:align :center :valign :middle}
   :system-health-red-hotspot-effort-cell          {:align :center :valign :middle}
   :system-health-defect-ratio-in-hotspots-cell    {:align :center :valign :middle}
   :system-health-table-header                     {:style "bold" :size 13 :color black}
   :system-health-table-label                      {:size 7 :leading 7 :spacing-after 3 :color black}
   :system-health-key-personnel                    {:size 15 :color black}
   :system-health-abandoned-code                   {:size 15 :color black}
   :system-health-code-health                      {:size 20 :leading 10 :color white}
   :system-health-last-code-health                 {:size 15 :leading 7 :color white}
   :system-health-last-year-code-health            {:size 15 :leading 7 :color white}
   :system-health-red-hotspot-ratio                {:size 15 :color black}
   :system-health-red-hotspot-effort               {:size 15 :color black}
   :system-health-defect-ratio-in-hotspots         {:size 15 :color black}})

(def pdf-report-metadata
  {:title         "CodeScene Status Report"
   :left-margin   70
   :right-margin  70
   :top-margin    75
   :bottom-margin 75
   :subject       "Report"
   :size          :a4
   :font          {:family "helvetica"}
   :author        "CodeScene"
   :creator       "CodeScene"
   :doc-header    ["CodeScene Status Report"]
   :header        nil
   :footer        false
   :pages         false
   :stylesheet    stylesheet})