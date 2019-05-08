(ns hotspots-x-ray.languages.java
  (:import javaparse.JavaMethodMiner))

(defn function-statistics-from
  [text]
  (map (fn [[name start end body]]
         {:name       name
          :start-line (Integer/parseInt start)
          :end-line   (Integer/parseInt end)
          :body       body})
       (JavaMethodMiner/methodsIn text)))
