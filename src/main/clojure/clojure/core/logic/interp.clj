(ns clojure.core.logic.interp
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.zip :as z]))

(def ^:dynamic *in-mplus* false)

(defn unwrap [g]
  (cond
   (trace-subst? g) g
   (= (mk-exp g) :inc) (recur (g))
   :else  g))

(declare tracer trace-subst?)

(defprotocol ISearchTree
  (tree [this]))

(deftype TraceSubstitutions [_tree]
  ISearchTree
  (tree [this] _tree)
  IBind
  (bind [this g]
    (let [exp (mk-exp g)
          ntree (cond
                (= exp :fresh)
                (-> _tree
                    (z/append-child [exp (mk-vars g)])
                    z/down
                    z/rightmost)
                (= exp :conde)
                (-> _tree
                    (z/append-child [exp])
                    z/down
                    z/rightmost)
                :else (z/append-child _tree exp))
          s (tracer ntree)
          s' (if (= exp :conde)
               (do
                 (println (z/root ntree))
                 (tracer (z/append-child ntree [:foo] #_(z/root (tree (unwrap (g (tracer))))))))
               (g s))]
      (if-not (trace-subst? s')
        (unwrap s')
        s')))

  IMPlus
  (mplus [this f]
    (let [s (f)
          s (if-not (trace-subst? s)
              (unwrap s)
              s)]
      (tracer (z/append-child _tree (z/root (tree s))))))

  ITake
  (take* [this]
    ))

(defn tracer
  ([] (tracer (z/vector-zip [])))
  ([tree]
     (TraceSubstitutions. tree)))

(defn trace-subst? [x]
  (instance? TraceSubstitutions x))

(comment
  ;; time to grok zippers
    
  (-> []
      z/vector-zip
      (z/insert-child [:fresh []])
      z/down
      (z/append-child [:conde])
      z/down
      z/rightmost
      (z/append-child [:fresh []])
      z/root)
  
  (z/root (tree (bind (bind (tracer) s#) s#)))

  (z/root (tree (bind (tracer) (fresh [x] s# s#))))

  (z/root (tree (bind (tracer) (fresh [x] (fresh [y] s#) s#))))

  (z/root
   (tree
    (bind (tracer)
          (fresh [x]
            (conde
              [s#]
              [s#])
            s#))))
  
  ;; 1. we care about fresh
  ;; 2. we care about conde
  ;; 3. if encounter -inc, we force it
  ;; 4. we never call goals we've seen before
  ;; 5. 

  ;; we 
  )