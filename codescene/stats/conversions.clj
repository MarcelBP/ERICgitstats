(ns codescene.stats.conversions)

(defn readable-minutes [duration-minutes]
  (let [int-minutes (int duration-minutes)
        years (quot int-minutes 525600)
        years-r (rem int-minutes 525600)
        weeks (quot years-r 10080)
        weeks-r (rem years-r 10080)
        days (quot weeks-r 1440)
        days-r (rem weeks-r 1440)
        hours (quot days-r 60)
        hours-r (rem days-r 60)
        minutes (rem hours-r 60)]
    (cond
      (pos? years) (str years "y" (when (pos? weeks) (str " " weeks "w")))
      (pos? weeks) (str weeks "w" (when (pos? days) (str " " days "d")))
      (pos? days) (str days "d" (when (pos? hours) (str " " hours "h")))
      (pos? hours) (str hours "h" (when (pos? minutes) (str " " minutes "m")))
      (pos? minutes) (str minutes "m")
      :else "0")))

