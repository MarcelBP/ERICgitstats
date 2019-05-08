(ns codescene.risks.by-architectural-component
  (:require [semantic-csv.core :as sc]
            [evolutionary-metrics.trends.dates :as dates]
            [codescene.mining.cleaning :as cleaning]
            [clj-time.core :as tc]))

;; This module calculates the delivery risk over the past week (sliding window).
;; This will be useful in the Architecture, Manager, and QA dashboard.
;; From a user perspective, they will see one extra column that says "Delivery Risk (weekly)".
;; That column shows either "Low", "Medium", "Warning (yellow triangle with a number, 7 or 8)", or
;; "Warning (red triangle with number, 0 or 10)".
;;
;; The risk is based on the commit risks over the past week (sliding window).
;; We look at which commits in the different groups (medium, warning yellow, warning red) that touch each component.
;;
;; This information helps a QA to focus correcting actions and verification to where they are likely to
;; pay off the most. Might also help a manager reason about delivery risk.

(def ^:cost risk-level-of-interest 7) ; lesser risks aren't relevant

(defn- risks-of-interest
  [risks]
  (filter (comp (partial <= risk-level-of-interest) :risk) risks))

(defn parse-risks-within-one-week
  [{:keys [age-time-now] :as _context} risk-by-commit]
  (let [cutoff-date (-> age-time-now dates/string->date (tc/minus (tc/weeks 1)))]
    (->> risk-by-commit
         (sc/cast-with {:risk sc/->int})
         risks-of-interest
         (remove (fn [{:keys [date]}] (tc/before? (dates/string->date date) cutoff-date))))))

(defn- parse-relevant-commits-from
  [architectural-commits risks]
  (let [relevant-commits (->> risks (map :commit) set)]
    (filter (comp relevant-commits :rev) architectural-commits)))

(def ^:const low-risk-placeholder "-") ; used generally for non-present values. We could display this in the UI as "normal/average".

(defn- commit->risk-from
  [risks]
  (let [lu (->> risks
                (map (juxt :commit :risk))
                (into {}))]
    (fn [commit]
      (get lu commit 0))))

(defn- highest-risk-for-each-component
  [risks cs]
  (let [commit->risk (commit->risk-from risks)]
    (->> cs
         (group-by :entity)
         (map (fn [[name cs-for-name]]
                (let [highest-risk (->> cs-for-name
                                        (map :rev)
                                        (map commit->risk)
                                        cleaning/safe-max)]
                  [name (if (= 0 highest-risk) low-risk-placeholder highest-risk)])))
         (into {}))))

(defn delivery-risk-for
  [context risk-by-commit architectural-commits scored-components]
  (let [risks (parse-risks-within-one-week context risk-by-commit)
        cs (parse-relevant-commits-from architectural-commits risks)
        risk-by-component (highest-risk-for-each-component risks cs)]
    (map (fn [{:keys [name] :as score}]
           (assoc score :deliveryrisk (get risk-by-component name low-risk-placeholder)))
         scored-components)))

(defn add-rolling-delivery-risk-for
  [context risk-by-commit-file architectural-commits-log scored-components]
  (delivery-risk-for context (sc/slurp-csv risk-by-commit-file) (sc/slurp-csv architectural-commits-log) scored-components))


