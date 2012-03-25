(defproject core.logic "0.6.9-SNAPSHOT" 
  :description "A logic/relational programming library for Clojure"
  :parent [org.clojure/pom.contrib "0.0.25"]
  :source-path "src/main/clojure"
  :Dependencies [[org.clojure/clojure "1.4.0-beta3"]]
  :dev-dependencies [[lein-swank "1.4.3"]
                     [lein-cljsbuild "0.1.3"]]
  :cljsbuild {:builds [{:source-path "src/main/cljs"
                        :compiler {:output-to "main.js"}}
                       {:source-path "src/test/cljs"
                        :optimizations :advanced
                        :compiler {:output-to "tests.js"}}]})
