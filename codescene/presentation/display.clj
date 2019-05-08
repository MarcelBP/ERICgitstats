(ns codescene.presentation.display
  "High-level display functions providing standardized
  formatting for dashboards: (css class names, etc.)")


(defn as-int
  "Convert string to int. Must be within integer boundaries"
  [s]
  (if (integer? s)
    s
    (Integer/parseInt s)))

(def ^:private placeholder-for-unknown-data "-")


(defn string-is-int?
  [s]
  (re-matches #"\d+" s))


(defn ->maybe-int
  "Convert to int when possible. Otherwise returns original input."
  [v]
  (if (integer? v)
    v
    (try
      (if (re-matches #"^\d+$" v)
        (Integer/parseInt v)
        v)
      (catch Exception e v))))


(defn to-ints-when-possible [row-map]
  (->> row-map 
       (map (fn [[k v]]
              [k (->maybe-int v)]))
       (into {})))


(defn health-of
  [field dashboard]
  (let [h (get dashboard field placeholder-for-unknown-data)]
    (if (string-is-int? h)
      h
      placeholder-for-unknown-data)))


(defn missed-goals->status [mg]
  (cond
    (= "-" mg) "not-calculated"
    (not (int? mg)) "not-calculated"
    (zero? mg) "good"
    ::otherwise "bad"))

(defn system-mastery-in
  [dashboard]
  (if-let [loss (get dashboard :knowledgelosspercentage)]
    (if (string-is-int? loss)
      (- 100 (as-int loss))
      placeholder-for-unknown-data)
    placeholder-for-unknown-data))


(defn missed-goals-display-mode [missed total]
  (cond
    (not (int? missed)) :no-goals
    (< 5  (count (str missed "/" total))) :missed-only
    ::otherwise-base-case :goals-and-total))
