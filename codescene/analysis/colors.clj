(ns codescene.analysis.colors)

(def ^:const color-for-knowledge-loss "red")
(def ^:const active-developers-team-color "LightSkyBlue")

;; A set of colors with maximum contrast according to the research by Kenneth Kelly, 1965:
(def ^:private kelly-colors
  ["FFB300"   ; Vivid Yellow
   "803E75"   ; Strong Purple
   "FF6800"   ; Vivid Orange
   "A6BDD7"   ; Very Light Blue
   "C10020"   ; Vivid Red
   "CEA262"   ; Grayish Yellow
   "817066"   ; Medium Gray
   "007D34"   ; Vivid Green
   "F6768E"   ; Strong Purplish Pink
   "00538A"   ; Strong Blue
   "FF7A5C"   ; Strong Yellowish Pink
   "53377A"   ; Strong Violet
   "FF8E00"   ; Vivid Orange Yellow
   "B32851"   ; Strong Purplish Red
   "F4C800"   ; Vivid Greenish Yellow
   "7F180D"   ; Strong Reddish Brown
   "93AA00"   ; Vivid Yellowish Green
   "593315"   ; Deep Yellowish Brown
   "F13A13"   ; Vivid Reddish Orange
   "232C16"]) ; Dark Olive Green

(def ^:const number-of-unique-colors (count kelly-colors))

;; We augment the color in the visualization - it's better to refactor this and use
;; the qualified hex value directly (need to change the Python part).
(def ^:const default-color "000000")
(def ^:const hex-default-color (str "#" default-color))

(defn any-colors-assigned?
  [devs]
  (boolean
   (some (fn [{:keys [color]}] (and (some? color)
                                    (not= color default-color)))
         devs)))

(defn add-contrast-colors-to
  [entities-to-color]
  (map (fn [entity distinct-color] (assoc entity :color distinct-color)) entities-to-color kelly-colors))
