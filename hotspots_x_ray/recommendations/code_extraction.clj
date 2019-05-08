(ns hotspots-x-ray.recommendations.code-extraction)

(defn- collect-function-bodies
  [{:keys [lines-left current-line bodies-with-stats]}
   {:keys [start-line end-line] :as fn-stat}]
  (let [leading-lines (- start-line current-line)
        start-lines (drop leading-lines lines-left)
        method-length (- end-line start-line)
        body-lines (take (inc method-length) start-lines)
        joined-body-lines (clojure.string/join "\n" body-lines)]
    {:lines-left (drop (inc method-length) start-lines)
     :current-line (inc end-line)
     :bodies-with-stats (conj bodies-with-stats (assoc fn-stat :body joined-body-lines))}))

(defn code-by-function-name
  [lines-in-file functions]
  (->> functions
       (reduce collect-function-bodies {:lines-left lines-in-file
                                        :current-line 1
                                        :bodies-with-stats []})
       :bodies-with-stats))