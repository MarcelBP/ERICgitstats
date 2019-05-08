(ns codescene.note-watch.utils
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [evolutionary-metrics.mining.file-patterns :as file-patterns]))

(def ^:const refactor-category 1)
(def ^:const supervise-category 2)
(def ^:const no-problem-category 3)

(def category-severity {"refactor" refactor-category
                        "supervise" supervise-category
                        "no-problem" no-problem-category
                        :no-category 4})

(defn read-notes-risks
  [notes-risks-file]
  (if (.exists (io/file notes-risks-file))
    (with-open [in-file (io/reader notes-risks-file)]
      (json/read in-file :key-fn keyword))
    {})) ; file doesn't exist

(defn non-resolved-hotspot?
  [notes-risks {:keys [relative-name repo-path] :as file}]
  (->> notes-risks
       (filter (fn [{:keys [warnings note-score]}]
                 (let [note (:note note-score)
                       name (or (:new-entity note)
                                (:last-entity note))]
                   (and (= name relative-name)
                        (= repo-path (:repo-path note))
                        (= (:category note) "no-problem")
                        (or (= (:biomarkers-warning warnings) "ok")
                            (= (:biomarkers-warning warnings) "not-calculated"))
                        (= (:trends-warning warnings) "ok")))))
       empty?))

(defn- virtual-filepath
  [note]
  (let [relative-path (or (:new-entity note) (:last-entity note))
        virtual-dir (file-patterns/final-segment (:repo-path note))]
    (file-patterns/add-repo-name virtual-dir relative-path)))

(defn notes-hotspot-candidates
  [notes-risks]
  (->> notes-risks
       (map (comp :note :note-score))
       (filter #(contains? #{"refactor" "supervise"} (:category %)))
       (map virtual-filepath)))

(defn note->note-with-virtual-root
  [note-risk]
  (let [n (:note (:note-score note-risk))
        vp (virtual-filepath n)]
    {:path vp
     :note note-risk}))

(defn notes-severity
  [notes-risks]
  (into {}
        (comp
         (map (comp :note :note-score))
         (map (juxt virtual-filepath
                    (comp category-severity :category))))
        notes-risks))

(defn note-severity-score
  [notes-severity name]
  (or (notes-severity name) (:no-category category-severity)))

(defn notes-biomarker-score
  [notes-risks]
  (map (fn [{:keys [note-score]}]
         {:name (virtual-filepath (:note note-score))
          :score (-> note-score :scores :biomarkers :new)})
       notes-risks))

(defn notes-with-category
  [notes-risks]
  (->> notes-risks
       (map (fn [{:keys [note-score]}]
              [(virtual-filepath (:note note-score))
               (-> note-score :note :category)]))
       (into {})))
