(ns codescene.analysis.diagnostics
  (:require [clojure.string :as string]
            [taoensso.timbre :as log]))

;; Code adapted from https://gist.github.com/michalmarczyk/492764
;; I've made the following modifications:
;; -  Use timbre rather than Clojure's tools library.
;; - Log all function arguments.
;; - Only trace public functions.

(defn trace-ns
  "Replaces each function from the given namespace with a version wrapped
  in a tracing call. Can be undone with untrace-ns. ns should be a namespace
  object or a symbol."
  [ns]
  (doseq [s (keys (ns-publics ns))
          :let [v (ns-resolve ns s)]
          :when (and (ifn? @v) (-> v meta :macro not))]
    (intern ns
            (with-meta s {:traced @v})
            (let [f @v] (fn [& args]
                          (let [pretty-printer (fn [arg]
                                                 ;; only shorten real seqs
                                                 ;; maps contain many options we want to see
                                                 (let [print-level (if (seq? arg) 20 100)]
                                                   (binding [*print-length* print-level]
                                                     (pr-str arg))))
                                traceable-args (string/join ", " (map pretty-printer args))]
                            (log/trace (str "Running: " s " with " (if (string/blank? traceable-args)
                                                                     "no args"
                                                                     traceable-args)))
                            (apply f args)))))))

(defn untrace-ns
  "Reverses the effect of trace-ns, replacing each traced function from the
  given namespace with the original, untraced version."
  [ns]
  (doseq [s (keys (ns-publics ns))
          :let [v (ns-resolve ns s)]
          :when (:traced (meta v))]
    (alter-meta! (intern ns s (:traced (meta v)))
                 dissoc :traced)))
