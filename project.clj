(defproject core.logic "0.7.4" 
  :description "A logic/relational programming library for Clojure"
  :extra-classpath-dirs ["checkouts/clojurescript/src/clj"
                         "checkouts/clojurescript/src/cljs"]
  :parent [org.clojure/pom.contrib "0.0.25"]
  :source-path "src/main/clojure"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.macro "0.1.1"]]
  :dev-dependencies [[lein-swank "1.4.4"]
                     [lein-cljsbuild "0.1.10"]]
  :cljsbuild {:builds {:test-simp {:source-path "src/test/cljs"
                                   :compiler {:optimizations :simple
                                              :static-fns true
                                              :output-to "tests_simple.js"}}
                       :test-adv {:source-path "src/test/cljs"
                                  :compiler {:optimizations :advanced
                                             :output-to "tests.js"}}}})
