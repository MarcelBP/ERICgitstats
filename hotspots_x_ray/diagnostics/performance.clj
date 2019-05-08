(ns hotspots-x-ray.diagnostics.performance
  (:require [taoensso.timbre :as timbre]))

(defmacro with-timbre-exe-time-info
  "Evaluates expr and logs the time it took to timbre as a side effect."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (timbre/trace (str "Elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))
