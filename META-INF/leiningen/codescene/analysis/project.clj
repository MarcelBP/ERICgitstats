(defproject codescene/analysis "0.2.305"
  :description "The shared CodeScene analysis, as a library."
  :url "https://github.com/empear-analytics/codescene-analysis"
  :license {:name "AllRightsReserved"}
  :repositories [["snapshots" {:url      "https://artifacts.internal.codescene.io/content/repositories/snapshots/"
                               :username [:env/artifacts_username :gpg]
                               :password [:env/artifacts_password :gpg]}]
                 ["releases" {:url      "https://artifacts.internal.codescene.io/content/repositories/releases/"
                              :username [:env/artifacts_username :gpg]
                              :password [:env/artifacts_password :gpg]}]]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/spec.alpha "0.2.176"]
                 [empear/evo-pattern-detector "0.4.9" :exclusions [clojure.core.async/bounded-count org.clojure/data.csv org.clojure/clojure]]
                 [com.taoensso/timbre "4.10.0"]
                 [slingshot "0.12.2"]
                 [clj-time "0.14.2"]
                 [digest "1.4.6"]
                 [semantic-csv "0.2.1-alpha1"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.antlr/antlr4-runtime "4.5.3"]
                 [org.jordanlewis/data.union-find "0.1.0"]

                 ; Used to process Rational Rose models in a custom X-Ray.
                 [tolitius/xml-in "0.1.0"]
                 [org.clojure/data.xml "0.2.0-alpha5"]

                 ;; Used by project management ns. HTTP calls should be removed
                 ;; from this library. PM data should be passed as input to the
                 ;; analysis instead.
                 [cheshire "5.8.0"]
                 [clj-http "3.7.0"]
                 [clj-pdf "2.3.2"]
                 [org.clojure/data.zip "0.1.2"]
                 [incanter/incanter-core "1.5.7"]
                 [incanter/incanter-io "1.5.7"]
                 [incanter/incanter-zoo "1.5.7"]
                 [incanter/incanter-charts "1.5.7"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [semantic-csv "0.2.1-alpha1"]
                 [stch-library/glob "0.3.0"]
                 [lock-key "1.5.0"]
                 [medley "1.0.0"]
                 [me.raynes/conch "0.8.0"]
                 [net.mikera/core.matrix "0.61.0" :exclusions [org.clojure/clojure]]
                 ; Stick to this older version of jmimemagic since version 0.1.5 introduced a
                 ; regression test bug in our suite: The binary file Key.snk was identified
                 ; as text.
                 [net.sf.jmimemagic/jmimemagic "0.1.3"
                  :exclusions [log4j]]
                 [com.github.albfernandez/juniversalchardet "2.1.0"]
                 ;; explicit dependency on jaxb-api for java 9 compatibility
                 ;; see https://stackoverflow.com/questions/43574426/how-to-resolve-java-lang-noclassdeffounderror-javax-xml-bind-jaxbexception-in-j
                 [javax.xml.bind/jaxb-api "2.3.0"]]

  :plugins [[lein-antlr "0.3.0"]]

  :source-paths ["src/analysis", "src/xray", "src/antlr"]
  :java-source-paths ["src/java" "src/generated"]
  :test-paths ["test/analysis" "test/xray"]

  :antlr-src-dir "src/antlr"
  :antlr-dest-dir "src/generated"
  :antlr-options {:Werror true
                  :visitor true
                  :package "hotspots_x_ray.languages.generated"}

  :profiles {:test           [:test-common :test-overrides]
             :test-overrides {}
             :test-common    [:dev-common]
             :dev            [:dev-common :dev-overrides]
             :dev-overrides  {}
             :dev-common     {:injections   [(require '[orchestra.spec.test :as stest])
                                             (stest/instrument)
                                             (.println System/err "Instrumented specs")
                                             (require 'taoensso.timbre)
                                             (taoensso.timbre/set-level! :info)
                                             (.println System/err "Set log level to :info")]
                              :dependencies [[org.clojure/test.check "0.9.0"]
                                             [orchestra "2017.11.12-1"]]}}
  
  :jvm-opts ["-Xmx8g" "-Djava.awt.headless=true" "-Xss512M"]

  :test-selectors {:no-regression-test   (complement :regression-test)
                   :only-regression-test :regression-test}

  :prep-tasks [["antlr"] ["javac"]]

  ;; workaround for http://dev.clojure.org/jira/browse/TCHECK-113
  :monkeypatch-clojure-test false

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version"
                   "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "v" "--no-sign"]
                  ["antlr"]
                  ["javac"]
                  ["install"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]])
