(ns codescene.analysis.architecture.aggregated-trends
  (:require [clojure.java.io :as io]
            [digest :as digest]
            [clj-time.core :as tc]
            [evolutionary-metrics.trends.dates :as dates]
            [taoensso.timbre :as log]
            [codescene.analysis.architecture.aggregator :as architectural-aggregator]
            [evolutionary-metrics.mergers.shared-mergers :as shared]
            [evolutionary-metrics.trends.aggregated-complexity-trends :as trends]
            [codescene.analysis.hotspot-complexity-trends :as hotspots-trends]
            [codescene.analysis.paths :as paths]
            [clojure.data.csv :as csv]))

(def ^:private architectural-trends-folder "architecture")

(defn trend-path
  [analysis-result-directory qualified-name]
  (->> (io/file (str (digest/sha-1 qualified-name) ".csv"))
       (io/file architectural-trends-folder)
       (io/file analysis-result-directory)
       .getPath))

(defn- input-files-for
  [analysis-path analysis-version analysis-file-names]
  (->> analysis-file-names
       (map (fn [f] {:analysis-result-directory analysis-path
                     :qualified-name f}))
       (map (partial hotspots-trends/complexity-trend-path analysis-version))
       (map clojure.java.io/file)
       (filter (fn [f] (.exists f))))) ; in case a previous step to calculate the trend failed we just ignore that input source

(defn- system-complexity-input-file-seq
  [analysis-path-fn input-folder]
  (->> input-folder
       analysis-path-fn
       clojure.java.io/file
       file-seq
       (filter (fn [f] (clojure.string/ends-with? (.getName f) ".csv")))))

(defn- aggregate-system-complexity
  [analysis-path-fn trend-sample-dates dest-file-name]
  (log/info "Calculating System Level Trend")
  (let [input-files (system-complexity-input-file-seq analysis-path-fn hotspots-trends/destination-folder)
        destination (analysis-path-fn (paths/analysis->result-path dest-file-name))]
    (trends/aggregate-trends-for input-files trend-sample-dates destination)))

(defn stack-trends-for
  [dates inputs]
  (let [names (map :name inputs)
        rows (into ["date"] names)
        trends (map :trend inputs)
        joined-trends (apply map (comp conj vector) trends)]
    {:headers rows
     :rows (map (fn [d ts] (into [d] ts)) dates joined-trends)}))

(defn- read-csv-column-as-int
  [n file-name]
  (map (fn [r] (Integer/parseInt (nth r n)))
       (shared/read-csv-sans-header-from file-name)))

(def ^:private complexity-column-index 1)
(def ^:private code-column-index 2)

(defn- stacked-trends
  [{:keys [analysis-path analysis-path-fn]} sample-dates component-names column-to-stack dest-file-name]
  (let [inputs (->> component-names (map (partial trend-path analysis-path)) (map (partial read-csv-column-as-int column-to-stack)))
        named-inputs (->> (map (fn [n row] {:name n :trend row}) component-names inputs))
        dates (map dates/date->string sample-dates)
        destination (analysis-path-fn (paths/analysis->result-path dest-file-name))
        {:keys [headers rows]} (stack-trends-for dates named-inputs)]
    (log/info "Generating stacked trends for architectural components: " (pr-str headers))
    (with-open [out-file (io/writer destination)]
      (csv/write-csv out-file [headers])
      (csv/write-csv out-file rows))))

(defn- aggregate-complexity-for
  [{:keys [analysis-path analysis-version]} trend-sample-dates name mapped-file-names]
  (let [result-dest (trend-path analysis-path name)
        input-files (input-files-for analysis-path analysis-version mapped-file-names)]
    (io/make-parents result-dest)
    (trends/aggregate-trends-for input-files trend-sample-dates result-dest)))

(defn- aggregate-architectural-complexity
  [{:keys [analysis-path-fn] :as context} trend-sample-dates transformations]
  (when (seq transformations)
    (log/info "Calculating the complexity of " (count transformations) " architectural components")
    (let [hotspots-log (analysis-path-fn paths/all-time-revisions-csv)
          files-by-group (architectural-aggregator/files-by-component hotspots-log :module transformations)
          unmapped-files (architectural-aggregator/umapped-files-in files-by-group)
          mapped-files (architectural-aggregator/remove-unmapped-files files-by-group)
          component-names (-> mapped-files keys sort)
          expected-names (map :transformation transformations)
          erroneous-mapping (clojure.set/difference (set component-names) (set expected-names))]
      (log/info "Skipping " (count unmapped-files) " number of unmapped files in the architectural complexity trends.")
      (when (seq erroneous-mapping) ; late detection, just warn
        (log/warn "Erroneous architectural mapping discovered (doesn't match any files). Component names: " (pr-str erroneous-mapping)))
      (doseq [[name mapped-file-names] mapped-files]
        (aggregate-complexity-for context trend-sample-dates name mapped-file-names))
      (log/info "Generating stacked trends for the mapped components: " (pr-str component-names))
      (if (seq component-names)
        (do
          (stacked-trends context trend-sample-dates component-names complexity-column-index paths/stacked-architectural-complexity-csv)
          (stacked-trends context trend-sample-dates component-names code-column-index paths/stacked-architectural-loc-csv))
        (log/warn "Looks like an error in your patterns for the architectural components. Your analysis is configured for architectural trends but we cannot find any data.")))))

(defn system-level-complexity-trend
  [{:keys [analysis-path-fn time-now] :as context}
   {:keys [analysis-start-date] :as _project}
   information-for-system-trends?
   architectural-transformations
   dest-file-name]
  (when information-for-system-trends?
    (let [analysis-interval (tc/interval (dates/string->date analysis-start-date)
                                         time-now)
          trend-sample-dates (trends/sample-points-in analysis-interval)]
      (log/info "Calculating System Level Trend with start date " analysis-start-date)
      (aggregate-system-complexity analysis-path-fn trend-sample-dates dest-file-name)
      (aggregate-architectural-complexity context trend-sample-dates architectural-transformations))))
