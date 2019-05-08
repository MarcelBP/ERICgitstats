(ns codescene.analysis.author-discovery
  (:require [evolutionary-metrics.app.pair-program-knowledge-mapper :as pair-programming]
            [codescene.analysis.specs :as specs]
            [semantic-csv.core :as sc]
            [clojure.spec.alpha :as s]
            [codescene.analysis.paths :as paths]))

(defn- read-unique-authors-from
  [f]
  (->> f sc/slurp-csv (map :author) set))

;; This is tricky since we might use different time stamps as the cut-off time
;; for the different analyses. See ENTERPRISE-500 for more details.
(defn all-authors-from
  [{:keys [analysis-path-fn individual-socal-log]} _project]
  (let [known-churners (read-unique-authors-from (analysis-path-fn paths/author-churn-csv))
        known-contributors (read-unique-authors-from individual-socal-log)]
    (clojure.set/union known-churners known-contributors)))

(defn- name->author [name]
  {:name name})

(defn- map-values [f xs]
  (into {} (for [[k v] xs]
             [k (f v)])))

(defn- authors->by-name [authors]
  ;; We would like to index by both :name and :email, but closed-maat
  ;; does not support emails.
  (map-values first (clojure.set/index authors [:name])))

(s/def ::unique-author-names (s/and set? (s/coll-of string?)))

(s/fdef merge-detected-authors-with-configured
        :args (s/cat :authors-in-analysis ::unique-author-names
                     :configured-authors ::specs/authors)
        :ret ::specs/authors)

(defn merge-detected-authors-with-configured
  [authors-in-analysis configured-authors]
  (let [sorted-configured-authors (->> configured-authors authors->by-name)
        all-existing-authors (->> authors-in-analysis
                                  (map name->author)
                                  authors->by-name)]
    (-> (merge-with merge all-existing-authors sorted-configured-authors)
        vals)))
