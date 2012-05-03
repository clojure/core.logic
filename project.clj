(defproject core.logic "0.7.2" 
  :description "A logic/relational programming library for Clojure"
  :parent [org.clojure/pom.contrib "0.0.25"]
  :source-path "src/main/clojure"
  :dependencies [[org.clojure/clojure "1.4.0-beta3"]
                 [org.clojure/tools.macro "0.1.1"]]
  :dev-dependencies [[lein-swank "1.4.3"]
                     [lein-cljsbuild "0.1.8"]]
  :cljsbuild {:builds [{:source-path "src/main/cljs"
                        :compiler {:output-to "main.js"}}
                       {:source-path "src/test/cljs"
                        :compiler {:optimizations :advanced
                                   :output-to "tests.js"}}]})
