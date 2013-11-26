(defproject org.clojure/core.logic "0.8.6-SNAPSHOT" 
  :description "A logic/relational programming library for Clojure"
  :parent [org.clojure/pom.contrib "0.0.25"]

  :jvm-opts ^:replace ["-Xmx512m" "-server"]

  :source-paths ["src/main/clojure"]

  :test-paths ["src/test/clojure"]

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2080"]
                 [org.clojure/tools.macro "0.1.2"]
                 [com.datomic/datomic-free "0.8.4270" :scope "provided"]]

  :plugins [[lein-cljsbuild "1.0.0"]]

  :cljsbuild
  {:builds
   [{:id "ws"
     :source-paths ["src/test/cljs"]
     :compiler {:optimizations :whitespace
                :static-fns true
                :output-to "tests.js"
                :output-dir "out"
                :source-map "tests.js.map"}}
    {:id "adv"
     :source-paths ["src/test/cljs"]
     :compiler {:optimizations :advanced
                :pretty-print false
                :output-to "tests.js"}}]})
