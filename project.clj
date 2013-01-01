(defproject org.clojure/core.logic "0.8.0-beta6-SNAPSHOT" 
  :description "A logic/relational programming library for Clojure"
  :parent [org.clojure/pom.contrib "0.0.25"]

  ;; lein 1
  :source-path "src/main/clojure"
  :test-path "src/test/clojure"
  :dev-dependencies [[lein-cljsbuild "0.2.9"]]
  ; :extra-classpath-dirs ["checkouts/clojurescript/src/clj"
  ;                        "checkouts/clojurescript/src/cljs"]

  ;; lein 2
  :source-paths ["src/main/clojure"
                 ;"clojurescript/src/clj"
                 ;"clojurescript/src/cljs"
                 ]
  :test-paths ["src/test/clojure"]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/clojurescript "0.0-1535"]
                 [org.clojure/google-closure-library "0.0-2029"]
                 [org.clojure/google-closure-library-third-party "0.0-2029"] 
                 [org.clojure/tools.macro "0.1.1"]
                 [org.clojure/tools.nrepl "0.2.0-RC1"]
                 [com.datomic/datomic-free "0.8.3551" :scope "provided"]]
  :plugins [[lein-cljsbuild "0.2.9"]]


  :cljsbuild {:builds {:test-simp {:source-path "src/test/cljs"
                                   :compiler {:optimizations :simple
                                              :pretty-print true
                                              :static-fns true
                                              :output-to "tests.js"}}
                       :test-adv {:source-path "src/test/cljs"
                                  :compiler {:optimizations :advanced
                                             :pretty-print true
                                             :output-to "tests.js"}}}})
