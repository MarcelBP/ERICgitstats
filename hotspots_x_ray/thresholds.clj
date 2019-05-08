(ns hotspots-x-ray.thresholds
  (:require [clojure.spec.alpha :as s]))

(def ^:private default-maat-options
  {:min-revs 10
   :min-shared-revs 10
   :min-coupling 20
   :max-coupling 100
   :max-changeset-size 10000}) ; really not applicable...how can we get rid of this?

(s/def ::xray-thresholds (s/keys :req-un
                                 [::min-revs
                                  ::min-shared-revs
                                  ::min-coupling
                                  ::max-coupling
                                  ::max-changeset-size]))

(defn ensure-defaults
  [thresholds]
  (merge default-maat-options thresholds))
