(defproject project-euler-clojure "0.1.0-SNAPSHOT"
  :description "Project Euler problems in clojure"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/tools.namespace "0.2.11"]]
  :main ^:skip-aot project-euler-clojure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
