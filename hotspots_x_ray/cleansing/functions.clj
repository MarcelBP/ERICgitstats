(ns hotspots-x-ray.cleansing.functions
  (:require [clojure.string :as string]))

(defn- combine-overloads
  [combiner-fn combined {:keys [name body]}]
  (if-let [existing (get combined name)]
    (assoc-in combined [name] (combiner-fn existing body))
    (assoc combined name body)))

(defn current-function-bodies-in
  ([current-version]
   (current-function-bodies-in current-version str))
  ([current-version combiner-fn]
   (reduce (partial combine-overloads combiner-fn) {} current-version)))

(defn- filter-function-from
  [splitted-content {:keys [start-line end-line]}]
  (->> splitted-content
       (drop (dec start-line))
       (take (- (inc end-line) start-line))))

(defn content-by-function-names
  "Returns a map with each function name as a key and the
   raw, unparsed (= newlines intact) contact as value.
   Overloads are combined."
  [content fns]
  (let [splitted-content (string/split-lines content)
        by-function-name (map (fn [f] {:name (:name f) :body (filter-function-from splitted-content f)}) fns)]
    (current-function-bodies-in by-function-name (comp vec concat))))