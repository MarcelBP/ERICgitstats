(ns codescene.stats.math
  (:require [incanter.stats :as stats]))

(defn- no-nan [d]
  (if (Double/isNaN d) 0.0 d))

(defn stats-for
  [vs]
  {:median (no-nan (stats/median vs))
   :mean   (no-nan (stats/mean vs))
   :sd     (no-nan (stats/sd vs))})
