(defproject org.clojure/core.logic "0.8.12-SNAPSHOT"
  :description "A logic/relational programming library for Clojure"
  :parent [org.clojure/pom.contrib "0.2.2"]

  :jvm-opts ^:replace ["-Xmx512m" "-server"]

  :source-paths ["src/main/clojure"]

  :test-paths ["src/test/clojure"]

  :dependencies [[org.clojure/clojure "1.7.0" :scope "provided"]
                 [org.clojure/clojurescript "0.0-3308" :scope "provided"]
                 [org.clojure/tools.analyzer.jvm "0.7.2"]
                 ;[com.datomic/datomic-free "0.8.4270" :scope "provided"]
                 ]

  :clean-targets ^{:protect false} ["resources/tests.js" "resources/out"]

  :plugins [[lein-cljsbuild "1.0.6"]]

  :cljsbuild
  {:builds
   [{:id "dev"
     :source-paths ["src/main/clojure/cljs" "src/test/cljs"]
     :compiler {:optimizations :none
                :output-to "resources/tests.js"
                :output-dir "resources/out-dev"
                :source-map true
                :verbose true
                :compiler-stats true}}
    {:id "simp"
     :source-paths ["src/main/clojure/cljs" "src/test/cljs"]
     :compiler {:optimizations :simple
                :static-fns true
                :output-to "resources/tests.js"
                :output-dir "resources/out-simp"
                :verbose true
                :compiler-stats true}}
    {:id "adv"
     :source-paths ["src/main/clojure/cljs" "src/test/cljs"]
     :compiler {:optimizations :advanced
                :output-to "resources/tests.js"
                :output-dir "resources/out-adv"
                :verbose true
                :compiler-stats true}}]})
