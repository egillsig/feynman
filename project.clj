(defproject feynman "0.1.0-SNAPSHOT"
  :description "Feynman: a Programming Language for Units of Measure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [instaparse "1.4.9"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.cli "0.3.7"]]
  :main ^:skip-aot feynman.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
