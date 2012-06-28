(defproject core.logic "0.7.5" 
  :description "A logic/relational programming library for Clojure"
  :extra-classpath-dirs ["checkouts/clojurescript/src/clj"
                         "checkouts/clojurescript/src/cljs"]
  :parent [org.clojure/pom.contrib "0.0.25"]
  :source-path "src/main/clojure"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.macro "0.1.1"]]
  :dev-dependencies [[lein-swank "1.4.4"]
                     [lein-cljsbuild "0.2.2"]]
  :cljsbuild {:builds {:test-simp {:source-path "src/test/cljs"
                                   :compiler {:optimizations :simple
                                              :pretty-print true
                                              :static-fns true
                                              :output-to "tests.js"}}
                       :test-adv {:source-path "src/test/cljs"
                                  :compiler {:optimizations :advanced
                                             :pretty-print true
                                             :output-to "tests.js"}}}})
