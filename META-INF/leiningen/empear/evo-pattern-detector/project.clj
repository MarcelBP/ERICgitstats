(defproject empear/evo-pattern-detector "0.4.9"
  :description "Machine learning algorithms to detect patterns in evolutionary metrics"
  :url "http://empear.com/"
  :license {:name "Empear AB"
            :url "http://www.empear.com/legal"}

  :signing {:gpg-key "Build Server <builds@empear.com>"}
  :repositories  [["snapshots" {:url "https://artifacts.internal.codescene.io/content/repositories/snapshots/"
                                :username  [:gpg :env/artifacts_username]
                                :password  [:gpg :env/artifacts_password]}]
                  ["releases" {:url "https://artifacts.internal.codescene.io/content/repositories/releases/"
                               :username  [:gpg :env/artifacts_username]
                               :password  [:gpg :env/artifacts_password]}]]

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/data.csv "0.1.4"]
                 [incanter/incanter-core "1.5.7" :exclusions [org.clojure/clojure]]
                 [empear/kmeans-clj "0.1.2"]
                 [org.clojure/data.json "0.2.6"]]
   :main evo-pattern-detector.core
   :aot [evo-pattern-detector.core]

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version"
                   "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "v" "--no-sign"]
                  ["install"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]])
