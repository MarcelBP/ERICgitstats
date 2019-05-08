(ns codescene.analysis.fractal-figures
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [evolutionary-metrics.mergers.shared-mergers :as shared]))

;;; This module is responsible for calculating the geometry of
;;; fractal figures.
;;;
;;; Instead of doing all the calculations client-side, we pre-calculate
;;; the layout of each figure. These are then visualized using D3 in
;;; the browser.
;;;
;;; This module uses input from an entity-ownership analysis with
;;; the following data per author and entity:
;;;
;;; :entity :author :author-added :added-ownership :author-deleted :deleted-ownership
;;;
;;; That input is translated to the following structure:
;;;
;;; entity,author,ownership,color,w,h,x,y
;;; some_file.c, Some Author, 0.33, red, 33, 100, 67, 0
;;; some_file.c, Another Author, 0.67, green, 67, 100, 0, 0
;;;
;;; To limit the amount of calculations, we only generate fractals
;;; for the top 100 hotspots (configurable).

(defn- entity-name
  [e]
  (nth e 0))

(defn- author-of
  [e]
  (nth e 1))

(defn- author-ownership
  [e]
  (nth e 2))

(def ^:const fractal-side 150.0)
(def ^:const fractal-area (* fractal-side fractal-side))

(defn side-length-from
  [ownership side-left]
  (/ (* ownership fractal-area)
     (max 1.0 side-left)))

(defn- as-rect
  [space-left width height]
  [width
   height
   (- fractal-side (first space-left))
   (- fractal-side (second space-left))])

(defn- y-fractal
  [ownership space-left]
  (let [[x-left y-left] space-left
        x-width (side-length-from ownership y-left)]
    {:rect (as-rect space-left x-width y-left)
     :space-left [(- x-left x-width) y-left]}))

(defn- x-fractal
  [ownership space-left]
  (let [[x-left y-left] space-left
        y-length (side-length-from ownership x-left)]
    {:rect (as-rect space-left x-left y-length)
     :space-left [x-left (- y-left y-length)]}))

(defn index-oriented-fractal
  [fractal-index ownership space-left]
  (if (even? fractal-index)
    (y-fractal ownership space-left)
    (x-fractal ownership space-left)))

(def ^:private default-color "black")

(defn- color-for
  [author colors]
  (get colors author default-color))

(defn- sort-rows-by-effort
  "Our algorithm is based on the fact that the rows
   are sorted based on effort (descending order) in
   order to calculate the correct x and y positions."
  [rows]
  (sort-by author-ownership > rows))

(defn- as-fractals
  [colors indexed-rows]
  (reduce (fn [{:keys [fractals space-left]} [index row]]
            (let [ownership (author-ownership row)
                  fractal-rect (index-oriented-fractal index ownership space-left)
                  author (author-of row)
                  fractal (into [(entity-name row) author ownership (color-for author colors)]
                                (:rect fractal-rect))]
              {:fractals (conj fractals fractal)
               :space-left (:space-left fractal-rect)}))
          {:fractals []
           :space-left [fractal-side fractal-side]}
          indexed-rows))

(defn effort->fractal
  "Expects a seq of vectors where each vector represents
   one author. The vectors must be sorted on effort (desc)."
  [efforts colors]
  (->> efforts
       sort-rows-by-effort
       (map-indexed vector)
       (as-fractals colors)
       :fractals))

;;
;; The API: read CSV input files.
;;

(defn- read-efforts
  [f {:keys [include-this-file?] :or {include-this-file? (constantly true)}}]
  (->> f
       shared/read-csv-sans-header-from
       (filter (comp include-this-file? first))
       (pmap (fn [[name author _author-added added-ownership _author-deleted _deleted-ownership]]
              [name author (Float/parseFloat added-ownership)]))))

(defn- read-author-colors
  [f]
  (->> f
       shared/read-csv-sans-header-from
       (pmap (fn [[name color _]] [name color]))
       (into {})))

(defn rows->effort-groups
  [rows]
  (group-by entity-name rows))

(defn efforts->fractals
  [efforts colors]
  (->> efforts
       rows->effort-groups
       vals
       (mapcat #(effort->fractal % colors))
       (into [])))

(defn generate-fractal-figures-with-options
  [fractal-options entity-ownership-file author-colors-file]
   (let [efforts (read-efforts entity-ownership-file fractal-options)
         colors (read-author-colors author-colors-file)]
     (efforts->fractals efforts colors)))

(defn generate-fractal-figures
  ([entity-ownership-file author-colors-file]
   (let [fractals (generate-fractal-figures-with-options {} entity-ownership-file author-colors-file)]
     (csv/write-csv *out* [["entity" "author" "ownership" "color" "w" "h" "x" "y"]])
     (csv/write-csv *out* fractals)))
  ([entity-ownership-file author-colors-file out-file-name]
   (with-open [out-file (io/writer out-file-name)]
     (binding [*out* out-file]
       (generate-fractal-figures entity-ownership-file author-colors-file)))))

(defn generate-team-level-fractal-figures
  ([sorted-fragmentation-file entity-ownership-file team-colors-file]
   (let [files-of-interest (->> sorted-fragmentation-file shared/read-csv-sans-header-from (map first) (into #{}))
         fractals (generate-fractal-figures-with-options {:include-this-file? files-of-interest}
                                                         entity-ownership-file
                                                         team-colors-file)]
     (csv/write-csv *out* [["entity" "author" "ownership" "color" "w" "h" "x" "y"]])
     (csv/write-csv *out* fractals)))
  ([sorted-fragmentation-file entity-ownership-file team-colors-file out-file-name]
   (with-open [out-file (io/writer out-file-name)]
     (binding [*out* out-file]
       (generate-team-level-fractal-figures sorted-fragmentation-file entity-ownership-file team-colors-file)))))
