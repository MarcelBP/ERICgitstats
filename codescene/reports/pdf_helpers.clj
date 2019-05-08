(ns codescene.reports.pdf-helpers
  (:refer-clojure :exclude [chunk])
  (:require [clojure.java.io :as io]
            [clj-time.local :as tl]
            [clj-time.format :as tf]
            [clojure.string :as str])
  (:import [java.awt Color Dimension]
           [com.lowagie.text Rectangle Image]
           (com.lowagie.text.pdf PdfContentByte PdfGraphics2D)
           (java.net URL)))

(defn entity-with-style [tag style-tags content]
  (let [all-tags (into [tag] style-tags)
        tag-with-style (keyword (str/join "." (map name all-tags)))]
    (into [tag-with-style] content)))

(defn table [style-tags & content]
  (entity-with-style :pdf-table style-tags content))

(defn cell [style-tags & content]
  (entity-with-style :pdf-cell style-tags content))

(defn paragraph [style-tags & content]
  (entity-with-style :paragraph style-tags content))

(defn phrase [style-tags & content]
  (entity-with-style :phrase style-tags content))

(defn chunk [style-tags & content]
  (entity-with-style :chunk style-tags content))

(defn image [style-tags & content]
  (entity-with-style :image style-tags content))

(defn graphics [style-tags & content]
  (entity-with-style :graphics style-tags content))

(defn chart [style-tags & content]
  (entity-with-style :chart style-tags content))

(defn pagebreak []
  [:pagebreak])

(defn row [& content]
  (into [] content))

(defn cells [cell-style-tags paragraph-style-tags names]
  (mapv (fn [n] (cell cell-style-tags (paragraph paragraph-style-tags n))) names))

(defn wrapped-cell [outer-cell-style-tags inner-cell-style-tags & content]
  "For making a cell wrapped in a table so padding can be used in outer-cell-style-tags"
  (cell outer-cell-style-tags
        (table [] [1] (row (entity-with-style :pdf-cell inner-cell-style-tags content)))))

(defn h-align [alignment left width img-left]
  (case alignment
    :left left
    :right (+ left (- width img-left))
    (+ left (/ (- width img-left) 2))))

(defn v-align [alignment bottom height img-height]
  (case alignment
    :bottom bottom
    :top (+ bottom (- height img-height))
    (+ bottom (/ (- height img-height) 2))))

(defn fit-image-fn [halignment valignment ^URL img-url]
  (fn [^Rectangle pos ^PdfContentByte canvas]
    (let [left (.getLeft pos)
          bottom (.getBottom pos)
          width (.getWidth pos)
          height (.getHeight pos)
          img (Image/getInstance img-url)]
      (.scaleToFit img width height)
      (let [img-width (.getScaledWidth img)
            img-height (.getScaledHeight img)
            img-left (h-align halignment left width img-width)
            img-bottom (v-align valignment bottom height img-height)]
        (.setAbsolutePosition img img-left img-bottom)
        (.addImage canvas img)))))

(defmacro ^:private with-saved-canvas-state [bindings & body]
  `(let ~bindings
     (.saveState ~(bindings 0))
     ~@body
     (.restoreState ~(bindings 0))))

(defn draw-using-g2d-fn [g2d-fn]
  (fn [^Rectangle pos ^PdfContentByte canvas]
      (let [width (.getWidth pos)
            height (.getHeight pos)
            left (.getLeft pos)
            bottom (.getBottom pos)
            g2d (PdfGraphics2D. canvas 595 842)]
        (with-saved-canvas-state
          [canvas canvas]
          (try
            (.translate g2d (int left) (int (- 842 bottom height)))
            (g2d-fn g2d width height)
            (finally (.dispose g2d)))))))
