(ns clojure.core.logic.par
  (:refer-clojure :exclude [==])
  (use clojure.core.logic))

;; fork-join wrapper from clojure.reducer

(defmacro ^:private compile-if
  [exp then else]
  (if (try (eval exp)
           (catch Throwable _ false))
    `(do ~then)
    `(do ~else)))

(compile-if
 (Class/forName "java.util.concurrent.ForkJoinTask")
 ;; We're running a JDK 7+
 (do
   (def pool (delay (java.util.concurrent.ForkJoinPool.)))

   (defn- fjtask [^Callable f]
     (java.util.concurrent.ForkJoinTask/adapt f))

   (defn- fjinvoke [f]
     (if (java.util.concurrent.ForkJoinTask/inForkJoinPool)
       (f)
       (.invoke ^java.util.concurrent.ForkJoinPool @pool ^java.util.concurrent.ForkJoinTask (fjtask f))))

   (defn- fjfork [task] (.fork ^java.util.concurrent.ForkJoinTask task))

   (defn- fjjoin [task] (.join ^java.util.concurrent.ForkJoinTask task)))
 ;; We're running a JDK <7
 (do
   (def pool (delay (jsr166y.ForkJoinPool.)))

   (defn- fjtask [^Callable f]
     (jsr166y.ForkJoinTask/adapt f))

   (defn- fjinvoke [f]
     (if (jsr166y.ForkJoinTask/inForkJoinPool)
       (f)
       (.invoke ^jsr166y.ForkJoinPool @pool ^jsr166y.ForkJoinTask (fjtask f))))

   (defn- fjfork [task] (.fork ^jsr166y.ForkJoinTask task))

   (defn- fjjoin [task] (.join ^jsr166y.ForkJoinTask task))))

;; parallel solvers

(declare dfs-par)

(defn dfs-par*
  ([]
     nil)
  ([node]
     (dfs-par node))
  [[node-a node-b]
   (let [task-b (fjfork (fjtask #(dfs-par node-b)))
         results-a (dfs-par node-a)
         results-b (fjjoin task-b)]
     (concat results-a results-b))])

(defn dfs-par [node]
  (fjinvoke
   #(let [rest-results (apply dfs-par* (children node))]
      (if-let [result (value node)]
        (cons result rest-results)
        rest-results))))

;; TODO bfs-par
