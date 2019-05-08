(ns codescene.analysis.visualization.enclosure-visualizer
  (:require [clojure.walk :as w]
            [clojure.spec.alpha :as s]))

(defn- contextual-paths-in
  "Each node in the resulting structure has to carry not only its name but also
   its path. For example {:name C :path A/B/C}."
  [direct-path]
  (->> direct-path
       (map-indexed (fn [i v] {:name v :path (take (inc i) direct-path)}))
       reverse))

(defn- as-hierarchy
  [leaf-data-fn {:keys [module meta-data]}]
  (let [parts (clojure.string/split module #"/")
        reversed-parts (reverse parts) ; A/B/C/f.c => (f.c C B A)
        file-name (first reversed-parts) ; f.c
        path (rest reversed-parts) ; reversed: (C B A)
        direct-path (reverse path)
        contextual-paths (contextual-paths-in direct-path)]
    (reduce (fn [acc contextual-path]
              {(assoc contextual-path :type :dir) acc})
            {{:name file-name :type :file}
             (leaf-data-fn meta-data parts file-name)}
            contextual-paths)))

(defn deep-merge-with [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))

(defn- hierarchical-file-names
  [leaf-data-fn files]
  (->> files
       (map (partial as-hierarchy leaf-data-fn))
       (apply (partial deep-merge-with merge))))

(defn- dir-node?
  [v]
  (and (map? v)
       (= :dir (:type v))))

(defn- leaf-node?
  [v]
  (and (map? v)
       (= :file (:type v))))

(defn- top-level?
  [v]
  (and (map? v)
       (every? map? (keys v))))

(defn- child-map->vector
  [[k v]]
  (cond

    (dir-node? k)
    {:name (:name k)
     :path (:path k)
     :children v}

    (leaf-node? k)
    v

    :else
    (throw (Exception. (str "Unknown node type in enclosure graph: " (pr-str k))))))

(defn- enclosureify
  [e]
  (if (top-level? e)
    (mapv child-map->vector e)
    e))

(s/fdef enclosure-format
        :args (s/cat :root-name string?
                     :spot-details-fn ifn?
                     :spots coll?)
        :ret map?)

(defn enclosure-format
  [root-name spot-details-fn spots]
  (->> spots
       (map (fn [spot] {:module (first spot)
                        :meta-data spot}))
       (hierarchical-file-names spot-details-fn)
       (w/prewalk enclosureify)
       (assoc {} :children)
       (merge {:name root-name :path []})))