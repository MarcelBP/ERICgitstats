(ns hotspots-x-ray.code-inspector
  (:require [hotspots-x-ray.languages.parser :as parser]
            [hotspots-x-ray.languages.specs :as parser-specs]
            [evolutionary-metrics.mining.vcs :as mining]
            [evolutionary-metrics.mining.specs :as mining-specs]
            [clojure.spec.alpha :as s])
  (:import (java.io IOException StringReader)))

(defn code-from
  "The parsed methods contain a body but all separating characters like
   whitespace and newlines are removed."
  [historic-content methods]
  {:pre [(string? historic-content)
         (s/valid? ::parser-specs/parsed-function-statistics methods)]}
  (let [all-code (clojure.string/split-lines historic-content)]
    (for [{:keys [start-line end-line]} methods
          :let [method-length (- end-line start-line)
                part (->> all-code (drop (dec start-line)) (take (inc method-length)) (clojure.string/join "\n"))]]
      part)))

(defn file-content-at-revision
  [git-cmd repo-path historic-revision]
  (let [[result content] (mining/show-file-version-at-revision git-cmd repo-path historic-revision)]
    (if result
      content
      (throw (IOException. (str "Failed to mine the file " (pr-str historic-revision) ", reason = " content))))))

(defn view-code-for
  [git-cmd repo-path {:keys [name] :as historic-revision} method-name]
  {:pre [(s/valid? ::mining-specs/git-exe-command git-cmd)
         (s/valid? ::mining-specs/repository-path repo-path)
         (s/valid? ::mining-specs/file-in-revision historic-revision)]
   :post [(string? %)]}
  (let [historic-content (file-content-at-revision git-cmd repo-path historic-revision)]
    (->> historic-content
         StringReader.
         (parser/parse-function-statistics name)
         (filter (comp (partial = method-name) :name))
         (code-from historic-content)
         (clojure.string/join "\n\n"))))
