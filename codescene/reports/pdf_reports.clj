(ns codescene.reports.pdf-reports
  "API for generating pdf-reports based on spec'd report data.
  The report data is a map with entries corresponding to sections in the report,
  where a nil entry means to exclude that section from the report."
  (:refer-clojure :exclude [chunk])
  (:require [codescene.reports.pdf-reports-def :as def]
            [clj-pdf.core :as pc :only [pdf]]
            [codescene.reports.pdf-helpers :as ph]
            [clojure.java.io :as io]
            [hotspots-x-ray.recommendations.code-health-markers :as code-health]
            [clojure.spec.alpha :as s]
            [codescene.reports.pdf-reports-spec :as reports-spec]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clj-time.local :as tl]
            [clj-time.format :as tf]
            [clojure.math.numeric-tower :as nt]
            [clojure.string :as str])
  (:import [java.awt Color]
           [com.lowagie.text Rectangle Image]
           (com.lowagie.text.pdf PdfContentByte)
           (java.net URL)))

(defn- awt-color [color]
  (let [[r g b] color]
    (Color. r g b)))

(defn- current-date-as-string []
  (tf/unparse (:year-month-day tf/formatters) (tl/local-now)))

(defn- nilable->time [date]
  (if (some? date)
    (str (tf/unparse (:year-month-day tf/formatters) date) " " (tf/unparse (:hour-minute tf/formatters) date))
    "-"))

(defn- nilable->percentage-str
  ([fraction decimals]
   (if (some? fraction)
     (str (Math/round (* 100 fraction)) "%")
     "-"))
  ([fraction]
   (nilable->percentage-str fraction 0)))

(defn- nilable->str [value]
  (if (some? value) (str value) "-"))

(defn split-first [re s]
  (string/split s re 2))

(defn- split-last [re s]
  (let [pattern (re-pattern (str re "(?!.*" re ")"))]
    (split-first pattern s)))


(defn- ^URL load-png-resource [dir name]
  (let [path (str dir "/" name ".png")]
    (io/resource path)))

(defn- code-health->cell-style [code-health]
  (if (nil? code-health)
    :code-health-unset-cell
    (cond
      (<= code-health def/code-health-low) :code-health-alarm-cell
      (>= code-health def/code-health-high) :code-health-ok-cell
      :else :code-health-warning-cell)))

(defn- code-health->trend-image [code-health last-code-health]
  (if (or (nil? code-health) (nil? last-code-health))
    "not-calculated"
    (let [diff (- ^Integer code-health ^Integer last-code-health)]
      (cond (>= diff def/code-health-trend-high) "big-up"
            (>= diff def/code-health-trend-low) "medium-up"
            (> diff 0) "small-up"
            (<= diff (- def/code-health-trend-high)) "big-down"
            (<= diff (- def/code-health-trend-low)) "medium-down"
            (< diff 0) "small-down"
            :else "stable"))))

(defn- code-health->image-fn [img-dir code-health last-code-health]
  (let [trend-image-name (code-health->trend-image code-health last-code-health)
        trend-image-resource (load-png-resource img-dir (str trend-image-name "-graph"))
        trend-image-good-resource (load-png-resource img-dir (str trend-image-name "-good-graph"))
        trend-image-fn (ph/fit-image-fn :center :bottom trend-image-resource)
        trend-image-good-fn (ph/fit-image-fn :center :bottom trend-image-good-resource)]
    (fn [^Rectangle pos ^PdfContentByte canvas]
      (when trend-image-resource (trend-image-fn pos canvas))
      (when trend-image-good-resource (trend-image-good-fn pos canvas)))))

(defn- team-autonomy->cell-style [team-autonomy]
  (case team-autonomy
    :high-team-autonomy :team-autonomy-alarm-cell
    :medium-team-autonomy :team-autonomy-neutral-cell
    :low-team-autonomy :team-autonomy-ok-cell
    :team-autonomy-unset-cell))

(defn- team-autonomy->str [team-autonomy]
  (case team-autonomy
    :high-team-autonomy "High"
    :medium-team-autonomy "Medium"
    :low-team-autonomy "Low"
    "-"))

(defn- hotspot-severity->color [severity]
  (case severity
    3 :hotspot-severity-alarm
    2 :hotspot-severity-warning
    :hotspot-severity-ok))

(defn- system-mastery->cell-style [system-mastery]
  (if (nil? system-mastery)
    :system-mastery-unset-cell
    (cond
      (>= system-mastery def/system-mastery-high) :system-mastery-ok-cell
      (< system-mastery def/system-mastery-low) :system-mastery-alarm-cell
      :else :system-mastery-warning-cell)))

(defn- delivery-risk->cell-style [delivery-risk]
  (if (nil? delivery-risk)
    :delivery-risk-unset-cell
    (cond
      (>= delivery-risk def/delivery-risk-high) :delivery-risk-alarm-cell
      (< delivery-risk def/delivery-risk-low) :delivery-risk-ok-cell
      :else :delivery-risk-warning-cell)))

(defn- missed-goals->cell-style [missed-goals]
  (if (nil? missed-goals)
    :missed-goals-unset-cell
    (if (zero? missed-goals)
      :missed-goals-ok-cell
      :missed-goals-alarm-cell)))

(defn- total-goals->str [total-goals]
  (if (some? total-goals) (str "/" (nilable->str total-goals) "")))

(defn- defects->cell-style [defects last-defects]
  ;; TODO: Colors
  (if (or (nil? defects) (nil? last-defects))
    :defects-unset-cell
    :defects-neutral-cell))

(defn- defects->trend-image [defects last-defects]
  (if (or (nil? defects) (nil? last-defects))
    "not-calculated"
    (let [diff (- ^Integer defects ^Integer last-defects)]
      (cond (> diff 0) "up-and-worse"
            (< diff 0) "down-and-better"
            :else "no-image"))))

(defn- resource-image-fn [img-dir img-name halign valign]
  (when-let [img (load-png-resource img-dir img-name)]
    (ph/fit-image-fn halign valign img)))

(defn- defects->trend-image-fn [img-dir defects last-defects]
  (when-let [trend-image (load-png-resource img-dir (defects->trend-image defects last-defects))]
    (ph/fit-image-fn :right :center trend-image)))


(defn- commits-last-month->activity-image [commits-last-month max-commits-last-month]
  (let [activity-percentage (when (and commits-last-month max-commits-last-month (> max-commits-last-month 0))
                              (int (* 100 (double (/ commits-last-month max-commits-last-month)))))]
    (cond
      (nil? activity-percentage) "not-calculated"
      (<= activity-percentage 20) "very-cold"
      (<= activity-percentage 40) "cold"
      (<= activity-percentage 60) "warm"
      (<= activity-percentage 80) "very-warm"
      :else "hot")))

(defn- sub-system-spacing-row []
  (mapv (fn [_] (ph/cell [:sub-system-spacing-cell] ""))
        (range 6)))

(defn- sub-system-health-row [sub-system options]
  (let [{:keys [sub-system team-autonomy system-mastery code-health last-code-health defects last-defects delivery-risk]} sub-system
        {:keys [img-dir]} options]
    (ph/row
      (ph/cell [:sub-system-name-cell]
               (ph/paragraph [:sub-system-name] sub-system))
      (ph/cell [:sub-system-team-autonomy-cell (team-autonomy->cell-style system-mastery)]
               (ph/paragraph [:sub-system-team-autonomy] (team-autonomy->str team-autonomy)))
      (ph/cell [:sub-system-mastery-cell (system-mastery->cell-style system-mastery)]
               (ph/paragraph [:sub-system-mastery] (nilable->percentage-str system-mastery)))
      (ph/cell [:sub-system-code-health-cell (code-health->cell-style code-health)]
               {:background-layer-fn (code-health->image-fn img-dir code-health last-code-health)}
               (ph/paragraph [:sub-system-code-health] (nilable->str code-health)))
      (ph/cell [:sub-system-defects-cell (defects->cell-style defects last-defects)]
               {:background-layer-fn (defects->trend-image-fn img-dir defects last-defects)}
               (ph/paragraph [:sub-system-defects] (nilable->str defects)))
      (ph/cell [:sub-system-delivery-risk-cell (delivery-risk->cell-style delivery-risk)]
               (ph/paragraph [:sub-system-delivery-risk] (nilable->str delivery-risk))))))

(defn- sub-system-health-table [sub-systems options]
  (let [columns ["Sub-System" "Team Autonomy" "System Mastery" "Code Health" "Defects" "Delivery Risks"]]
    (concat
      (ph/table [:sub-system-health-table] [3 1 1 1 1 2])
      [(ph/cells [:sub-system-health-header-cell] [:sub-system-health-header] columns)]
      (interpose (sub-system-spacing-row)
                 (map #(sub-system-health-row % options) sub-systems)))))

(defn- hotspot-detail-paragraph [detail]
  (let [{:keys [description severity]} detail]
    (ph/paragraph [:hotspot-details] (ph/chunk [(hotspot-severity->color severity)] "• ") description)))

(defn- hotspot-row [{:keys [file code-health last-code-health hotspot-details comment] :as _hotspot}]
  (let [parts (split-last #"/" file)
        file-name (second parts)
        dir-name (first parts)
        details (string/join "\n" (map :description hotspot-details))]
    (ph/row
      (ph/cell [:hotspot-name-cell]
               (ph/paragraph [:hotspot-file-name] file-name)
               (ph/paragraph [:hotspot-dir-name] dir-name))
      (ph/cell [:hotspot-code-health-cell (code-health->cell-style code-health)]
               (ph/paragraph [:hotspot-code-health] (nilable->str code-health)))
      (ph/cell [:hotspot-last-code-health-cell (code-health->cell-style last-code-health)]
               (ph/paragraph [:hotspot-last-code-health] (nilable->str last-code-health)))
      (apply ph/cell [:hotspot-details-cell]
               (map hotspot-detail-paragraph hotspot-details))
      (ph/cell [:hotspot-comment-cell]
               (ph/paragraph [:hotspot-comment] (nilable->str comment))))))

(defn- hotspots-table [hotspots options]
  (let [timespan (:timespan options)
        last-score-header (if (= :month timespan) "Last Month" "Last Year")
        columns ["Hotspot" "Code Health" last-score-header "Details" "Comment"]]
    (concat
      (ph/table [:hotspots-table] [35 10 10 25 20])
      [(ph/cells [:hotspots-table-header-cell] [:hotspots-table-header] columns)]
      (map hotspot-row hotspots))))

(defn- title-page-graphics [g2d]
  (doto g2d
    ; Coordinates for a4, 72 dpi (595x842)
    (.setColor (Color. 235 73 80))
    (.fillRect (int 384) (int 0) (int 595) (int 518))
    (.setColor (Color. 248 238 113))
    (.fillRect (int 0) (int 569) (int 151) (int 842))))

(defn- draw-pie [g2d width height ratio]
  (let [angle (* ratio 360)
        size (min width height)
        left (max 0 (/ (- width height) 2))
        top (max 0 (/ (- height width) 2))]
    (doto g2d
      (.setColor (awt-color def/black))
      (.fillArc (int left) (int top) (int size) (int size) (int 90) (int angle))
      (.setColor (awt-color def/light-grey))
      (.fillArc (int left) (int top) (int size) (int size) (int (+ 90 angle)) (int (- 360 angle))))))

(defn- pie-fn [nilable-ratio]
  (let [ratio (or nilable-ratio 0)]
    (fn [g2d width height]
      (draw-pie g2d width height ratio))))

(defn- title-page [options]
  (let [{:keys [title date] :or {title "New Report" date (current-date-as-string)}} options]
    [(ph/graphics [] {:under true} title-page-graphics)
     (ph/paragraph [:title] title)
     (ph/paragraph [:subtitle] "Behavioral Code Analysis")
     (ph/paragraph [:subtitle] date)
     (ph/paragraph [:subtitle] "by CodeScene")
     (ph/pagebreak)]))

(defn- system-health-key-personnel-table [system-health _options]
  (let [{:keys [abandoned-code key-personnel key-personnel-ratio]} system-health]
    (ph/table [:system-health-key-personnel-table] [1]
              (ph/row
                (ph/cell [:system-health-table-header-cell]
                         (ph/paragraph [:system-health-table-header] "Key Personnel")))
              (ph/row
                (ph/cell [:system-health-key-personnel-image-cell]
                         {:background-layer-fn (ph/draw-using-g2d-fn (pie-fn key-personnel-ratio))} ""))
              (ph/row
                (ph/cell [:system-health-key-personnel-cell]
                         (ph/paragraph [:system-health-key-personnel] (nilable->percentage-str key-personnel-ratio 1))))
              (ph/row
                (ph/cell [:system-health-table-label-cell]
                         (ph/paragraph [:system-health-table-label] (str "Code written by " key-personnel " developers"))))
              (ph/row
                (ph/cell [:system-health-abandoned-code-image-cell]
                         {:background-layer-fn (ph/draw-using-g2d-fn (pie-fn abandoned-code))} ""))
              (ph/row
                (ph/cell [:system-health-abandoned-code-cell]
                         (ph/paragraph [:system-health-abandoned-code] (nilable->percentage-str abandoned-code 1))))
              (ph/row
                (ph/cell [:system-health-table-label-cell]
                         (ph/paragraph [:system-health-table-label] "Abandoned code"))))))

(defn- system-health-code-health-table [system-health _options]
  (let [{:keys [code-health last-month-code-health last-year-code-health]} system-health]
    (ph/table [:system-health-code-health-table] [1]
              (ph/row
                (ph/cell [:system-health-table-header-cell]
                         (ph/paragraph [:system-health-table-header] "Code Health")))
              (ph/row
                (ph/cell [:system-health-table-label-cell]
                         (ph/paragraph [:system-health-table-label] "Status Now")))

              (ph/row
                (ph/wrapped-cell [:system-health-code-health-table-cell]
                                 [:system-health-code-health-cell (code-health->cell-style code-health)]
                                 (ph/paragraph [:system-health-code-health] (nilable->str code-health))))
              (ph/row
                (ph/cell [:system-health-table-label-cell]
                         (ph/paragraph [:system-health-table-label] "Last Month")))
              (ph/row
                (ph/wrapped-cell [:system-health-last-code-health-table-cell]
                                 [:system-health-last-code-health-cell (code-health->cell-style last-month-code-health)]
                                 (ph/paragraph [:system-health-last-code-health] (nilable->str last-month-code-health))))
              (ph/row
                (ph/cell [:system-health-table-label-cell]
                         (ph/paragraph [:system-health-table-label] "Last Year")))
              (ph/row
                (ph/wrapped-cell [:system-health-last-year-code-health-table-cell]
                                 [:system-health-last-year-code-health-cell (code-health->cell-style last-year-code-health)]
                                 (ph/paragraph [:system-health-last-year-code-health] (nilable->str last-year-code-health)))))))

(defn- system-health-descriptive-data-table [system-health options]
  (let [{:keys [red-hotspot-ratio red-hotspot-effort defect-ratio-in-hotspots]} system-health
        {:keys [img-dir]} options]
    (ph/table [:system-health-descriptive-data-table] [1]
              (ph/row
                (ph/cell [:system-health-table-header-cell]
                         (ph/paragraph [:system-health-table-header] "Descriptive Data")))
              (ph/row
                (ph/cell [:system-health-hotspot-logo-cell]
                         {:background-layer-fn (resource-image-fn img-dir "hotspotlogo" :center :middle)} ""))
              (ph/row
                (ph/cell [:system-health-red-hotspot-ratio-cell]
                         (ph/paragraph [:system-health-red-hotspot-ratio]
                                       (nilable->percentage-str red-hotspot-ratio 1))))
              (ph/row
                (ph/cell [:system-health-table-label-cell]
                         (ph/paragraph [:system-health-table-label] "Red Hotspots")))
              (ph/row
                (ph/cell [:system-health-red-hotspot-effort-cell]
                         (ph/paragraph [:system-health-red-hotspot-effort]
                                       (nilable->percentage-str red-hotspot-effort 1))))
              (ph/row
                (ph/cell [:system-health-table-label-cell]
                         (ph/paragraph [:system-health-table-label] "Development Effort in Red Hotspots")))
              (ph/row
                (ph/cell [:system-health-defect-ratio-in-hotspots-cell]
                         (ph/paragraph [:system-health-defect-ratio-in-hotspots]
                                       (nilable->percentage-str defect-ratio-in-hotspots 1))))
              (ph/row
                (ph/cell [:system-health-table-label-cell]
                         (ph/paragraph [:system-health-table-label] "Of Estimated Bugfixes in Red Hotspots"))))))

(defn- system-health-section [system-health options]
  (when system-health
    [(ph/paragraph [:heading] "System Health")
     (ph/table [:system-health-table] [1 1 1]
               (ph/row
                 (system-health-key-personnel-table system-health options)
                 (system-health-code-health-table system-health options)
                 (system-health-descriptive-data-table system-health options)))
     (ph/pagebreak)]))

(defn- refactoring-section
  [{:keys [details] :as _refactoring}]
  [(ph/paragraph [:body] details)])

(defn- missed-goal-row [goal options]
  (let [{:keys [file goal-details]} goal
        {:keys [img-dir]} options
        parts (split-last #"/" file)
        file-name (second parts)
        dir-name (first parts)]
    (ph/row
      (ph/cell [:missed-goal-image-cell]
               {:background-layer-fn (resource-image-fn img-dir "warning-triangle-redstandalone" :center :middle)} "")
      (ph/cell [:missed-goal-name-cell]
               (ph/phrase [:missed-goal-file-name] file-name)
               (ph/phrase [:missed-goal-dir-name] dir-name))
      (ph/cell [:missed-goal-details-cell]
               (ph/phrase [:missed-goal-details] goal-details)))))

(defn- missed-goals-table [missed-goals options]
  (let [columns ["" "File" "Details"]]
    (concat
      (ph/table [:missed-goals-table] [1 5 5])
      [(ph/cells [:missed-goals-table-header-cell] [:missed-goals-table-header] columns)]
      (map #(missed-goal-row % options) missed-goals))))

(defn- goals-section [goals options]
  (let [{:keys [missed-goals total-goals missed-refactorings missed-supervisions missed-no-problems]} goals]
    (when goals
     (concat [(ph/paragraph [:heading] "Supervised Goals")
              (ph/paragraph [:body]
                            (cond
                              (or (nil? total-goals) (zero? total-goals)) "No goals defined."
                              (or (nil? missed-goals) (zero? missed-goals)) (str total-goals " goals, all on track!")
                              :else (str total-goals " goals, " (- total-goals missed-goals) " on track.")))]
             (if (seq missed-refactorings)
               [(ph/paragraph [:heading2] "Missed Goals: Planned Refactorings")
                (missed-goals-table missed-refactorings options)])
             (if (seq missed-supervisions)
               [(ph/paragraph [:heading2] "Missed Goals: Supervised Hotspots")
                (missed-goals-table missed-supervisions options)])
             (if (seq missed-no-problems)
               [(ph/paragraph [:heading2] "Missed Goals: Previously Marked as No Problem")
                (missed-goals-table missed-no-problems options)])
             [(ph/pagebreak)]))))


(defn- project-summary-row [project-name max-commits-last-month system-health options]
  (let [{:keys [abandoned-code active-authors analysis-time commits-last-month _former-developers
                code-health last-code-health lines-of-code missed-goals total-goals delivery-risk]} system-health
        {:keys [img-dir]} options
        system-mastery (when abandoned-code (- 1.0 abandoned-code))
        activity-image-name (commits-last-month->activity-image commits-last-month max-commits-last-month)]
    [(ph/table [:project-table] [1 5 5 4 2]
               (ph/row
                 (ph/cell []
                          (if-let [activity-image (load-png-resource img-dir activity-image-name)]
                            (ph/image [:project-activity-image] activity-image)
                            ""))
                 (ph/cell []
                          (ph/table [:project-description-and-status-table] [1]
                                    (ph/row
                                      (ph/cell [:project-name-cell]
                                               (ph/paragraph [:project-name] project-name)))
                                    (ph/row
                                      (ph/cell [:project-commits-last-month-cell]
                                               (ph/paragraph [:project-commits-last-month] (str (nilable->str commits-last-month) " commits in the last month"))))))
                 (ph/cell []
                          (ph/table [:project-data-table] [1]
                                    (ph/row
                                      (ph/cell [:project-analysis-time-cell]
                                               (ph/paragraph [:project-analysis-time] (ph/chunk [:project-table-label] "Last analysis:  ") (nilable->time analysis-time))))
                                    (ph/row
                                      (ph/cell [:project-lines-of-code-cell]
                                               (ph/paragraph [:project-lines-of-code] (ph/chunk [:project-table-label] "Lines of code:  ") (nilable->str lines-of-code))))
                                    (ph/row
                                      (ph/cell [:project-active-developers-cell]
                                               (ph/paragraph [:project-active-developers] (ph/chunk [:project-table-label] "Active developers:  ") (nilable->str active-authors))))))
                 (ph/cell []
                          (ph/table [:project-health-table] [1 1 1]
                                    (ph/row
                                      (ph/cell [:project-table-label]
                                               (ph/paragraph [] "System mastery"))
                                      (ph/cell [:project-table-label]
                                               (ph/paragraph [] "Delivery risk?"))
                                      (ph/cell [:project-table-label]
                                               (ph/paragraph [] "Missed goals")))
                                    (ph/row
                                      (ph/cell [:project-system-mastery-cell (system-mastery->cell-style system-mastery)]
                                               (ph/paragraph [:project-system-mastery] (nilable->percentage-str system-mastery)))
                                      (ph/cell [:project-delivery-risk-cell (delivery-risk->cell-style delivery-risk)]
                                               (ph/paragraph [:project-delivery-risk] (nilable->str delivery-risk)))
                                      (ph/cell [:project-missed-goals-cell (missed-goals->cell-style missed-goals)]
                                               (ph/paragraph [:project-missed-goals]
                                                             (ph/chunk [] (nilable->str missed-goals))
                                                             (ph/chunk [:project-total-goals] (total-goals->str total-goals)))))))
                 (ph/cell []
                          (ph/table [:project-main-health-table] [1]
                                    (ph/row
                                      (ph/cell [:project-table-label]
                                               (ph/paragraph [] "Code health")))
                                    (ph/row
                                      (ph/cell [:project-code-health-cell (code-health->cell-style code-health)]
                                               {:background-layer-fn (code-health->image-fn img-dir code-health last-code-health)}
                                               (ph/paragraph [:project-code-health] (nilable->str code-health))))))))]))

(defn- early-warnings-text [{:keys [title description early-warning-details]}]
  ;TODO: How to present this ...
  [(ph/paragraph [:body]
                 (str title "-" description "-" (string/join "," early-warning-details)))])

(defn- early-warnings-section [early-warnings]
  (when (some? early-warnings)
    (concat
      [(ph/paragraph [:heading] "Early Warnings")]
      (if (seq early-warnings)
        (mapcat early-warnings-text early-warnings)
        [(ph/paragraph [:body] "No early warnings have been detected.")])
      [(ph/pagebreak)])))

(comment
  "Goals supervision section"
  "No goals defined"
  "X goals, all on track"
  "5 goals, 3 on track"
  "Refactorings: [number] [file]"
  )
(defn- weeks-and-days-str [days]
  (let [weeks-part (quot days 7)
        days-part (rem days 7)]
    (if (> weeks-part 0)
      (str weeks-part "w " days-part "d")
      (str days-part "d"))))

(defn- delivery-risk-text [{:keys [branch-name lead-time risk nbr-of-commits]}]
  ;TODO: ...
  [(ph/paragraph [:body]
                 (str "Branch " branch-name " with " nbr-of-commits " commits and a lead-time of " (weeks-and-days-str lead-time) " has an increased delivery risk (" risk ")."))])

(defn- delivery-risks-section [delivery-risks]
  (when (some? delivery-risks)
    (concat
      [(ph/paragraph [:heading] "Delivery Risks")]
      (if (seq delivery-risks)
        (mapcat delivery-risk-text delivery-risks)
        [(ph/paragraph [:body] "No delivery risks have been detected.")])
      [(ph/pagebreak)])))

(defn- development-output-chart [development-output-over-time]
  [(ph/chart []
             {:x-label     "Date"
              :y-label     "Commits/Authors"
              :time-series true
              :title       "Development Output"
              :type        :line-chart
              :time-format "yyyy-MM-dd"
              :background  [255 255 255]
              :vector      true
              :translate   [(* def/page-width 0.125) (* def/page-height 0.15)]
              :width       (* def/page-width 0.75)
              :height      (* def/page-width 0.5)}
             (->> development-output-over-time
                  (map (fn [x] [(:date x) (:commits-per-week x)]))
                  (into ["Commits Per Week"]))
             (->> development-output-over-time
                  (map (fn [x] [(:date x) (:authors-per-week x)]))
                  (into ["Authors Per Week"])))])

(defn- active-authors-chart [active-authors-over-time]
  [(ph/chart []
             {:x-label     "Date"
              :y-label     "Commits/Authors"
              :time-series true
              :title       "Active Authors"
              :type        :line-chart
              :time-format "yyyy-MM-dd"
              :background  [255 255 255]
              :vector      true
              :translate   [(* def/page-width 0.125) (* def/page-height 0.6)]
              :width       (* def/page-width 0.75)
              :height      (* def/page-width 0.5)}
             (->> active-authors-over-time
                  (map (fn [x] [(:date x) (:authors-per-month x)]))
                  (into ["Authors Per Month"])))])

(defn- system-trends-section [system-trends]
  (when (some? system-trends)
    (let [{:keys [development-output-over-time active-authors-over-time]} system-trends]
      (concat
        [(ph/paragraph [:heading] "System Trends")]
        (if (some? development-output-over-time)
          (development-output-chart development-output-over-time)
          [(ph/paragraph [:body] "No development output data found.")])
        (if (some? active-authors-over-time)
          (active-authors-chart active-authors-over-time)
          [(ph/paragraph [:body] "No active authors data found.")])))))

(defn- subsystem-health-section [subsystem-health options]
  (when (some? subsystem-health)
    [(ph/paragraph [:heading] "System Health and Risks")
     (if (seq subsystem-health)
       (sub-system-health-table subsystem-health options)
       (ph/paragraph [:body] "No subsystems have been configured."))
     (ph/pagebreak)]))

(defn- system-hotspots-section [hotspots options]
  (when (some? hotspots)
    [(ph/paragraph [:heading] "Hotspots")
     (if (seq hotspots)
       (hotspots-table hotspots options)
       (ph/paragraph [:body] "No hotspots have been found."))
     (ph/pagebreak)]))

(defn- subsystem-hotspots-section [subsystem hotspots options]
  (when (some? hotspots)
    [(ph/paragraph [:heading2] (str "Hotspots For " subsystem))
     (if (seq hotspots)
       (hotspots-table hotspots options)
       (ph/paragraph [:body] "No hotspots have been found."))
     (ph/pagebreak)]))

(defn- hotspots-by-subsystem-section [hotspots-by-subsystem options]
  (when (some? hotspots-by-subsystem)
    (concat
      [(ph/paragraph [:heading] "Hotspots By Sub-System")]
      (if (seq hotspots-by-subsystem)
        (mapcat (fn [[subsystem hotspots]] (subsystem-hotspots-section subsystem hotspots options)) hotspots-by-subsystem)
        [(ph/paragraph [:body] "No subsystems have been configured.")
         (ph/pagebreak)]))))

(defn- add-key-for-value [old-key new-key form]
  (let [value (old-key form)]
    (if (some? value)
      (assoc form new-key value)
      form)))

(defn- pick-last-values [report-data timespan]
  (let [last-value-key (if (= timespan :month) :last-month-code-health :last-year-code-health)]
    (walk/postwalk #(add-key-for-value last-value-key :last-code-health %) report-data)))

(defn- with-optional-metadata [pdf-report-metadata options]
  (if-let [font (:font options)]
    (assoc pdf-report-metadata :font {:encoding :unicode :ttf-name font})
    pdf-report-metadata))

(defn project-report-data [report-data options]
  "Generate data for pdf generation as specified by clj-pdf"
  (let [{:keys [timespan] :or {timespan :month}} options
        {:keys [system-health early-warnings delivery-risks goals system-trends
                subsystem-health system-hotspots hotspots-by-subsystem]} (pick-last-values report-data timespan)]

    (concat
      [(with-optional-metadata def/pdf-report-metadata options)]
      (title-page options)
      (system-health-section system-health options)
      (goals-section goals options)
      (system-trends-section system-trends)
      (subsystem-health-section subsystem-health options)
      (system-hotspots-section system-hotspots options)
      (hotspots-by-subsystem-section hotspots-by-subsystem options)
      (early-warnings-section early-warnings)
      (delivery-risks-section delivery-risks))))

(defn- max-commits-last-month [report-datas-by-project]
  (->> (vals report-datas-by-project)
       (map :system-health)
       (map :commits-last-month)
       (filter some?)
       (sort >)
       first))

(defn- sort-by-commits-last-month [report-datas-by-project]
  (let [accessor (fn [key]
                   (let [report-data (get report-datas-by-project key)
                         system-health (:system-health report-data)
                         commits-last-month (:commits-last-month system-health)]
                     [commits-last-month key]))]
    (into (sorted-map-by (fn [key1 key2] (compare (accessor key2) (accessor key1))))
          report-datas-by-project)))

(defn management-report-data [report-datas-by-project options]
  "Generate data for pdf generation as specified by clj-pdf"
  (let [max-commits-last-month (max-commits-last-month report-datas-by-project)
        report-datas-with-last-values (pick-last-values report-datas-by-project :month)
        sorted-report-datas (sort-by-commits-last-month report-datas-with-last-values)]
    (concat
      [(with-optional-metadata def/pdf-report-metadata options)]
      (title-page options)
      (mapcat
        (fn [[project-name report-data]]
          (project-summary-row project-name max-commits-last-month (:system-health report-data) options))
        sorted-report-datas))))

(defn project-report [report-data options out]
  "Takes a map of data, a map of options, and produces a pdf-report.
  `out` can be either a file name or an output stream"
  (pc/pdf (project-report-data report-data options) out))

(s/fdef project-report
        :args (s/cat :report-data ::reports-spec/report-data
                     :options ::reports-spec/report-options
                     :out some?))


(defn management-report [report-datas-by-project options out]
  "Takes a map of maps of data by name, a map of options, and produces a pdf-report.
  `out` can be either a file name or an output stream"
  (pc/pdf (management-report-data report-datas-by-project options) out))

(s/fdef management-report
        :args (s/cat :report-datas-by-project ::reports-spec/report-datas-by-project
                     :options ::reports-spec/report-options
                     :out some?))