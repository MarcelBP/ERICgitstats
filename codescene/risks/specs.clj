(ns codescene.risks.specs
  (:require [clojure.spec.alpha :as s]
            [evolutionary-metrics.mining.specs :as mining-spec]
            [codescene.analysis.specs :as analysis-spec]))

(s/def ::git-commit-range-mine-fn (s/fspec :args (s/cat :range ::mining-spec/git-revisions)
                                           :ret ::mining-spec/mined-git-log))

(s/def ::dir ::analysis-spec/analysis-path)

(s/def ::analysis-result (s/keys :req-un [::dir]))

;; The results of a risk prediction analysis.
;;

(comment
  {:commit commit-hash
   :risk-classification 7
   :risks [{:message "Modifies Hotspots"
            :details "Modifies the Hotspots: Compiler.cpp, Jit.cpp, Gc.cpp"}
           {:message "Modifies critical parts of Hotspots"
            :details "Modifes critical parts of Hotspots: Compiler::big_bad_hotspot, Jit::another_hotspot_method"}
           {:message "Increases complexity"
            :details "Increases the Complexity/LoC factor of Compiler.cpp"}
           {:message "Parallel Development"
            :details "2 other developers have modified: Compiler.cpp"}]})

(s/def ::commit ::mining-spec/rev)

(s/def ::risk-classification (s/int-in 1 10))

(s/def ::message string?)
(s/def ::details string?)
(s/def ::risk (s/keys :req-un [::message ::details]))
(s/def ::risks (s/coll-of ::risk))

(s/def ::delivery-risk (s/keys :req-un [::commit ::risk-classification ::risks]))