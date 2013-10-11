(defproject org.clojure/core.logic "0.8.5-SNAPSHOT" 
  :description "A logic/relational programming library for Clojure"
  :parent [org.clojure/pom.contrib "0.0.25"]

  :jvm-opts ^:replace ["-Xmx512m" "-server"]

  :source-paths ["src/main/clojure"]
  
  :test-paths ["src/test/clojure"]

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1934"]
                 [org.clojure/tools.macro "0.1.2"]
                 [com.datomic/datomic-free "0.8.4218" :scope "provided"]]

  :plugins [[lein-cljsbuild "0.3.3"]]

  :cljsbuild
  {:builds
   [{:id "simple"
     :source-paths ["src/test/cljs"]
     :compiler {:optimizations :simple
                :pretty-print true
                :static-fns true
                :output-to "tests.js"}}
    {:id "adv"
     :source-paths ["src/test/cljs"]
     :compiler {:optimizations :advanced
                :pretty-print true
                :output-to "tests.js"}}]})
