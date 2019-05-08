(defproject empear/kmeans-clj "0.1.2"
  :description "Fork of https://github.com/codyrioux/kmeans-clj"
  :url "http://empear.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :signing {:gpg-key "Build Server <builds@empear.com>"}
  :repositories  [["snapshots" {:url "https://artifacts.internal.codescene.io/content/repositories/snapshots/"
                                :username  [:gpg :env/artifacts_username]
                                :password  [:gpg :env/artifacts_password]}]
                  ["releases" {:url "https://artifacts.internal.codescene.io/content/repositories/releases/"
                               :username  [:gpg :env/artifacts_username]
                               :password  [:gpg :env/artifacts_password]}]]

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.3.465"]
                 [incanter/incanter-core "1.5.7" :exclusions [org.clojure/clojure]]]

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version"
                   "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "v" "--no-sign"]
                  ["install"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]])
