(defproject cacsremoteservice "3.0.13"
  :description "A remote analysis service"
  :url "https://github.com/empear-analytics/empear-enterprise"
  :min-lein-version "2.0.0"
  :repositories [["snapshots" {:url      "https://artifacts.internal.codescene.io/content/repositories/snapshots/"
                               :username [:env/artifacts_username :gpg]
                               :password [:env/artifacts_password :gpg]}]
                 ["releases" {:url      "https://artifacts.internal.codescene.io/content/repositories/releases/"
                              :username [:env/artifacts_username :gpg]
                              :password [:env/artifacts_password :gpg]}]]
  :dependencies [[org.clojure/clojure "1.10.1-beta2"] ; needed for fix https://dev.clojure.org/jira/browse/CLJ-2484?page=com.atlassian.jira.plugin.system.issuetabpanels:comment-tabpanel#issue-tabs
                 [org.clojure/spec.alpha "0.2.176"]
                 ;; explicit dependency on jaxb-api for java 9 compatibility
                 ;; see https://stackoverflow.com/questions/43574426/how-to-resolve-java-lang-noclassdeffounderror-javax-xml-bind-jaxbexception-in-j
                 [javax.xml.bind/jaxb-api "2.3.0"]
                 [compojure "1.5.2"]
                 [ring/ring-defaults "0.2.2"]
                 [ring/ring-json "0.4.0"]
                 [ring-middleware-format "0.7.0"]
                 [cheshire "5.8.0"]
                 [http-kit "2.2.0"]
                 [slingshot "0.12.2"]
                 [clj-http "3.4.1" :exclusions [org.apache.httpcomponents/httpclient]]
                 [digest "1.4.5"]
                 [org.clojure/data.xml "0.2.0-alpha5"]
                 [overtone/at-at "1.2.0"]
                 [codescene/analysis "0.2.305"
                  :exclusions [cheshire
                               clj-http
                               com.fasterxml.jackson.core/jackson-core
                               com.fasterxml.jackson.dataformat/jackson-dataformat-cbor
                               com.fasterxml.jackson.dataformat/jackson-dataformat-smile
                               com.taoensso/encore
                               com.taoensso/timbre
                               com.taoensso/truss
                               commons-io
                               io.aviso/pretty
                               org.apache.httpcomponents/httpclient
                               org.apache.httpcomponents/httpcore
                               org.clojure/clojure
                               org.clojure/core.memoize
                               org.clojure/tools.analyzer
                               org.clojure/tools.analyzer.jvm
                               org.clojure/tools.cli
                               org.clojure/tools.reader
                               xerces/xercesImpl]]
                 ;; use explicit xerces dependency to avoid clashes between different transitive dependencies' versions
                 ;; necessary for proper run on Java 9
                 [xerces/xercesImpl "2.11.0"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [ragtime/ragtime "0.6.3" :exclusions [org.clojure/java.jdbc]]
                 [yesql "0.5.3" :exclusions [instaparse]]
                 [com.h2database/h2 "1.4.193"]
                 [selmer "1.11.7" :exclusions [joda-time]]
                 [clj-time "0.12.2"]
                 [org.python/jython-standalone "2.7.0"]
                 [bouncer "1.0.0"]
                 [org.clojure/core.async "0.3.465" :exclusions [org.clojure/tools.reader
                                                                org.clojure/core.cache]]
                 [com.taoensso/timbre "4.7.4" :exclusions [org.clojure/tools.reader]]
                 [com.cemerick/friend "0.2.3"]
                 [org.apache.httpcomponents/httpclient "4.5.3"]
                 [clj-aws-s3 "0.3.10" :exclusions [joda-time]]
                 ;; slf4j-timbre and jcl-over-slf4j is used to be able to configure logging for 3rd party libraries
                 ;; e.g. commons-logging is used by jmimemagic (MagicParser) library
                 [com.fzakaria/slf4j-timbre "0.3.6"]
                 [org.slf4j/jcl-over-slf4j "1.7.25"]
                 [timbre-ns-pattern-level "0.1.2"]

                 ;; proxy settings autodetection: https://github.com/MarkusBernhardt/proxy-vole
                 ;; explicit xtend lib dependency due to broken checksum of version 2.8.3 in maven central
                 [com.github.markusbernhardt/proxy-vole "1.0.4"
                  :exclusions [org.eclipse.xtend/org.eclipse.xtend.lib]]
                 [org.eclipse.xtend/org.eclipse.xtend.lib "2.8.4"]
                 [buddy/buddy-sign "2.2.0"]
                 [com.taoensso/nippy "2.13.0"]
                 [cprop "0.1.11"]
                 [etaoin/etaoin "0.2.8"]
                 [org.clojars.pntblnk/clj-ldap "0.0.15"]
                 [org.julienxx/clj-slack "0.5.6"]
                 [com.climate/claypoole "1.1.4"]
                 ;; handy utility functions - also used by compojure
                 [medley "1.0.0"]
                 [com.draines/postal "2.0.3"]
                 ;; new Java 11+ compatible version of 'ordered' library used transitively by ring-middleware-format
                 ;; see https://www.deps.co/blog/how-to-upgrade-clojure-projects-to-use-java-11/#java-util-collection-toarray
                 [org.flatland/ordered "1.5.7"]
                 ]

  :plugins [[lein-ring "0.8.13" :exclusions [org.clojure/clojure]]
            [lein-uberwar "0.1.0" :exclusions [org.clojure/clojure org.clojure/data.xml leinjacker]]
            [lein-project-version "0.1.0"]
            [lein-shell "0.5.0"]]

  :ring {:handler cacsremoteservice.handler/app-routes
         :init    cacsremoteservice.handler/start-service
         :destroy cacsremoteservice.handler/stop-service}

  ;; Since we distribute our application to the end customers,
  ;; we want to compile everything and remove sources

  :uberwar {:aot :all
            :omit-source true
            :handler cacsremoteservice.handler/app-routes
            :init    cacsremoteservice.handler/start-service
            :destroy cacsremoteservice.handler/stop-service
            :name    "codescene-enterprise-edition.war"}
  :uberjar-name "codescene-enterprise-edition.standalone.jar"

  :profiles {:test           [:test-common :test-overrides]
             :test-overrides {}
             :test-common    [:dev-common]
             :dev            [:dev-common :dev-overrides]
             :dev-overrides  {}
             :dev-common     {:source-paths ["dev"]
                              :injections   [(require '[orchestra.spec.test :as stest])
                                             (stest/instrument)
                                             (.println System/err "Instrumented specs")]
                              :dependencies [[org.clojure/tools.namespace "0.2.11"]
                                             [javax.servlet/servlet-api "2.5"]
                                             [ring/ring-mock "0.3.0"]
                                             [org.clojure/test.check "0.9.0"]
                                             [com.gfredericks/test.chuck "0.2.9"]
                                             [orchestra "2017.11.12-1"]]
                              :plugins      [[jonase/eastwood "0.2.4"]
                                             [lein-kibit "0.1.5"]]}
             :uberjar {:aot :all
                       :omit-source true}}

  :test-selectors {:default (complement :integration)
                   :integration :integration}

  :repl-options {:init-ns user}

  ;; Note that these options are only applicable to leiningen (REPL) - uberjar/uberwar don't get them!
  ;; allowAttachSelf is necessary for Dynamic Attach on Java 9+ (e.g. for clj-async-profiler: https://github.com/clojure-goes-fast/clj-async-profiler#usage)
  :jvm-opts ["-Dconf=my-config.edn" "-XX:-OmitStackTraceInFastThrow" "-Djdk.attach.allowAttachSelf=true"]

  :main cacsremoteservice.main
  :aot [cacsremoteservice.main]

  :aliases {"release" ["uberjar" "uberwar"]}

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version"
                   "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "v" "--no-sign"]
                  ["shell" "make" "archives"]
                  ["install"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]])
