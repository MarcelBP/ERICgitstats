(ns codescene.presentation.branches
  (:require [codescene.presentation.display :as display]
            [codescene.branches.branch-statistics :as branch-statistics]
            [codescene.presentation.health :as health]
            [codescene.stats.conversions :as conversion]
            [clj-time.core :as tc]
            [clojure.spec.alpha :as s]
            [evolutionary-metrics.trends.dates :as dates]
            [semantic-csv.core :refer [slurp-csv] :as sc]))


(defn- calc-ttl-minutes
  [{:keys [start end] :as branch}]
  (let [start-time (dates/date-time-string->date start)
        end-time  (dates/date-time-string->date end)]
    (assoc branch :ttl-minutes (tc/in-minutes (tc/interval start-time end-time)))))

(defn- ttl-minutes->percentage
  [max-ttl-minutes {:keys [ttl-minutes] :as branch}]
  (assoc branch :ttl-percentage (int (* 100 (/ ttl-minutes max-ttl-minutes)))))

(defn- calc-delivery-risk-status
  [{:keys [risk] :as branch}]
  (assoc branch :delivery-risk-status (health/delivery-risk-val->status risk)))


(def ^:private delivery-risk-minimum 7)

(s/def ::risk int?)
(s/def ::ttl-minutes int?)
(s/def ::ttl-percentage (s/and int? #(<= 0 % 100)))
(s/def ::delivery-risk-status ::health/delivery-risk)

(s/def ::maybe-string-integer (s/and string? #(re-matches #"^\d+$" %)))
(s/def ::string string?)

;;;  These specs need to be less restrictive than their ::branch-statistics counterparts
(s/def ::leadmerge (s/or :analysis ::branch-statistics/leadmerge :maybe-string-integer ::maybe-string-integer))        
(s/def ::ttl       (s/or :analysis ::branch-statistics/ttl       :maybe-string-integer ::string))              
(s/def ::commits   (s/or :analysis ::branch-statistics/commits   :maybe-string-integer ::maybe-string-integer))          
(s/def ::authors   (s/or :analysis ::branch-statistics/authors   :maybe-string-integer ::maybe-string-integer))          

(s/def ::presentation-branch-keys
  (s/keys
    :req-un [::ttl-minutes
             ::ttl-percentage
             ::delivery-risk-status]))

(s/def ::relaxed-branch-statistic
  (s/keys
    :req-un [::risk
             ::ttl
             ::leadmerge
             ::commits
             ::authors

             ::branch-statistics/ttlwarning
             ::branch-statistics/authorwarning
             ::branch-statistics/merged
             ::branch-statistics/repository
             ::branch-statistics/name
             ::branch-statistics/start
             ::branch-statistics/end
             ::branch-statistics/hasriskprediction]))
(s/def ::relaxed-branch-statistics (s/coll-of ::relaxed-branch-statistic))

(s/def ::presentation-branch (s/merge ::relaxed-branch-statistic ::presentation-branch-keys))


(defn parse-slurped-branch-csv [slurped-csv]
  (->> slurped-csv
       (sc/cast-with {:merged sc/->boolean
                      :ttl sc/->int
                      :hasriskprediction sc/->int
                      :leadmerge sc/->int
                      :risk sc/->int
                      :commits sc/->int
                      :authors sc/->int})
       (map (fn [b] (update b :ttl #(conversion/readable-minutes (* 60 %)))))))
(s/fdef parse-slurped-branch-csv
  :ret ::relaxed-branch-statistics)


(defn top-delivery-risks
  [branch-stats]
  (let [reverse-compare (comp - compare)
        branches-with-ttl-minutes (->> branch-stats
                                       (map (fn [b] (update b :risk display/as-int)))
                                       (remove (fn [b] (>  delivery-risk-minimum (:risk b))))
                                       (map calc-ttl-minutes)
                                       (sort (fn [a b]
                                               (let [risk-compare (reverse-compare (:risk a) (:risk b))]
                                                 (if (zero? risk-compare)
                                                   (reverse-compare (:ttl-minutes a) (:ttl-minutes b))
                                                   risk-compare)))))
        max-ttl-minutes (apply max 0 (map :ttl-minutes branches-with-ttl-minutes))]
    (->> branches-with-ttl-minutes
         (take 4)
         (map (partial ttl-minutes->percentage max-ttl-minutes))
         (map calc-delivery-risk-status))))

(s/fdef top-delivery-risks
  :args (s/cat :branch-stats ::relaxed-branch-statistics)
  :ret (s/coll-of ::presentation-branch))


