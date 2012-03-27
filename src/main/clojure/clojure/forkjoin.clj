(ns clojure.forkjoin
  (:import [java.util.concurrent RecursiveTask 
                                 ForkJoinPool]))

(set! *warn-on-reflection* true)

;; -----------------------------------------------
;; Helpers to provide an idiomatic interface to FJ

(defprotocol IFJTask
  (fork [this])
  (join [this])
  (run [this])
  (compute [this]))

(deftype FJTask [^RecursiveTask task]
  IFJTask
  (fork [_] (FJTask. (.fork task)))
  (join [_] (.join task))
  (run [_] (.invoke task))
  (compute [_] (.compute task)))

(defn ^FJTask task* [f]
  (FJTask. (proxy [RecursiveTask] []
             (compute [] (f)))))

(defmacro task [& rest]
  `(task* (fn [] ~@rest)))

(defprotocol IFJPool
  (shutdown [this])
  (submit [this task])
  (invoke [this task])
  (execute [this task]))

(deftype FJPool [^ForkJoinPool fjp]
  IFJPool
  (shutdown [this] (.shutdown this))
  (submit [this task]
          (let [^FJTask task task]
            (.submit fjp 
              ^RecursiveTask (.task task))))
  (invoke [this task]
          (let [^FJTask task task]
            (.invoke fjp 
              ^RecursiveTask (.task task))))
  (execute [this task]
           (let [^FJTask task task]
             (.execute fjp 
               ^RecursiveTask (.task task)))))

(defn ^FJPool fjpool
  ([] (FJPool. (ForkJoinPool.)))
  ([n] (FJPool. (ForkJoinPool. n))))

;; -----------------------------------------------
;; Fib

#_(def pool (fjpool))

(defn fib [n]
  (if (<= n 1)
    n
    (let [f1 (fork (task (fib (dec n))))]
      (+ (run (task (fib (- n 2))))
         (join f1)))))

(comment
  (invoke pool (task (fib 10)))
  )
