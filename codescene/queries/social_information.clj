(ns codescene.queries.social-information
  (:require [codescene.analysis.visualization.parse-analysis-results :as parse]
            [codescene.analysis.paths :as paths]
            [codescene.analysis.visualization.default-values :as default-values]
            [semantic-csv.core :as sc]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]))

(defn- main-dev-from
  [devs name]
  (:owner (get devs name default-values/ownership)))

(defn team-autonomy-from
  [teams team-fragmentation name]
  (let [main-team (main-dev-from teams name)]
    (if (or (= main-team "_unassigned_")
            (= main-team "_unmodified_"))
      "-"
      (if-let [f (get team-fragmentation name)]
        (cond
          (< f 0.25) "High"
          (< f 0.5)  "Medium"
          :else "Low")
        "-"))))

(defn- add-density-if-exists
  [results name defect-density]
  (let[d (get defect-density name {:density "-" :defects "-"})]
    (-> results
        (assoc :defectratio (:density d))
        (assoc :ndefects (:defects d)))))

(defn parse-fragmentation
  "Copy-pasted and tweaked as we want to cast the fractal value to double, but without breaking all regression tests."
  [f]
  (->> f
       sc/slurp-csv
       (sc/cast-with {:fractal-value sc/->double})
       (map (fn [{:keys [entity fractal-value]}] [entity fractal-value ]))
       (into {})))

(defn- parse-defects
  "The defect data might not be availabe."
  [defect-file]
  (if (.exists (io/file defect-file))
    (parse/defect-density defect-file)
    {}))

(def ^:const inconclusive-knowledge-loss "-")

(defn- known-knowledge-loss?
  [loss]
  (not (and (string? loss)
            (= inconclusive-knowledge-loss loss))))

(s/def ::knowledge-loss (s/or :inconclusive #(not (known-knowledge-loss? %))
                              :known (s/and known-knowledge-loss? (partial >= 100))))

(s/def ::losses map?)
(s/def ::name string?)

(s/fdef loss-for
        :args (s/cat :losses ::losses
                     :name    ::name)
        :ret ::knowledge-loss)

(defn- loss-for
  [losses name]
  (if-let [loss-info (get losses name)]
    (let [{:keys [inconclusive loss]} loss-info]
      (if inconclusive
        inconclusive-knowledge-loss
        (Math/round (* 100 loss))))
    inconclusive-knowledge-loss))

(defn system-mastery-of
  [losses name]
  (let [loss (loss-for losses name)]
    (if (known-knowledge-loss? loss)
      (max 0 (- 100 loss))
      inconclusive-knowledge-loss)))

(defn for-file
  [analysis-path name]
  (let [p (partial paths/as-child-path analysis-path)
        main-devs (parse/knowledge (p paths/social-main-dev))
        knowledge-loss (parse/knowledge-loss (p paths/knowledge-loss-csv))
        defect-density (parse-defects (p paths/defect-density-by-file-csv))
        teams (parse/knowledge (p paths/team-knowledge-csv))
        team-fragmentation (parse-fragmentation (p paths/fragmentation-team-csv-file-name))]
    (let [results {:maindev       (main-dev-from main-devs name)
                   :knowledgeloss (loss-for knowledge-loss name)
                   :mainteam      (main-dev-from teams name)
                   :teamautonomy  (team-autonomy-from teams team-fragmentation name)}]
      (add-density-if-exists results name defect-density))))
