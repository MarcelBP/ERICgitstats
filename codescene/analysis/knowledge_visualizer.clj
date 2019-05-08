(ns codescene.analysis.knowledge-visualizer
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [codescene.analysis.colors :as colors]))

(defn- teams-by-id-for
  [teams]
  (->> teams
       (map (fn [{:keys [id name color]}]
              [id {:name  name
                   :color color}]))
       (into {})))

(defn- generate-team-colors-file!
  [teams-by-id dest-file-name]
  (with-open [dest-file (io/writer dest-file-name)]
    (let [team-colors (map (fn [{:keys [name color]}]
                             [name (str "#" (or color colors/default-color))])
                           (vals teams-by-id))]
      (csv/write-csv dest-file [["author" "color"]])
      (csv/write-csv dest-file team-colors))))

(defn- generate-team-knowledge-colors
  "Generates a CSV file where each team is the author and its color
   is the one assinged by the user in the UI configuration."
  [team-color-file teams]
  (let [teams-by-id (teams-by-id-for teams)]
    (generate-team-colors-file! teams-by-id team-color-file)))

(def ^:private default-team "_unassigned_")

(defn- generate-colors-for-developers!
  [analysis-path-fn
   {:keys [author-colors-output-file-name]}
   authors]
  (let [colored-devs (map (fn [{:keys [name color team]}]
                            [name (str "#" (or color colors/default-color)) (or team default-team)])
                          authors)]
    (with-open [out-file (io/writer (analysis-path-fn author-colors-output-file-name))]
      (csv/write-csv out-file [["author" "color" "team"]])
      (csv/write-csv out-file colored-devs))))

(def ^:const consider-ex-devs-a-team "Ex-Developers")
(def ^:const active-developers-team "Active Developers")

(defn- generate-knowledge-loss-developers!
  [analysis-path-fn
   {:keys [knowledge-loss-color active-developer-color lost-authors-output-file-name lost-authors-team-output-file-name]}
   authors]
  (let [ex-developers (map (fn [{:keys [name]}]
                             [name knowledge-loss-color consider-ex-devs-a-team])
                           (filter :exdev authors))]
    (with-open [out-file (io/writer (analysis-path-fn lost-authors-output-file-name))]
      (csv/write-csv out-file [["author" "color" "team"]])
      (csv/write-csv out-file ex-developers))
    (with-open [out-file (io/writer (analysis-path-fn lost-authors-team-output-file-name))]
      (csv/write-csv out-file [["author" "color"]])
      (csv/write-csv out-file [[consider-ex-devs-a-team knowledge-loss-color] [active-developers-team active-developer-color]]))))

(defn- generate-knowledge-colors-for-teams!
  [analysis-path-fn {:keys [team-colors-output-file-name]} teams]
  (let [team-colors-dest (analysis-path-fn team-colors-output-file-name)]
    (generate-team-knowledge-colors team-colors-dest teams)))

(defn generate-knowledge-colors!
  [{:keys [analysis-path-fn]}
   knowledge-color-spec
   authors
   teams]
  (generate-colors-for-developers! analysis-path-fn knowledge-color-spec authors)
  (generate-knowledge-loss-developers! analysis-path-fn knowledge-color-spec authors)
  (generate-knowledge-colors-for-teams! analysis-path-fn knowledge-color-spec teams))
