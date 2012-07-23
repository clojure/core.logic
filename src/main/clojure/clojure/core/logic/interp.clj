(ns clojure.core.logic.interp
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))

;; to remove thunks
(defn unwrap [g]
  (cond
   (trace-subst? g) g
   (= (mk-exp g) :inc) (recur (g))
   :else  g))

(declare tracer trace-subst?)

(defprotocol ISearchTree
  (tree [this]))

(deftype TraceSubstitutions [_tree seen]
  ISearchTree
  (tree [this] _tree)

  IBind
  (bind [this g]
    (if (seen (class g))
      (tracer (conj _tree [:goal :seen]) seen)
      (let [exp (mk-exp g)
            sub-tree (cond
                      (= exp :fresh) [exp (mk-vars g)]
                      (= exp :conde) []
                      :else :goal)
            sub-s (tracer sub-tree (conj seen (class g)))
            new-tree (if (= exp :conde)
                       (conj _tree [:conde (-> (g sub-s) unwrap tree)])
                       (conj _tree (-> (g sub-s) unwrap tree)))]
        (tracer new-tree))))

  IMPlus
  (mplus [this f]
    (let [s (unwrap (f))]
      (tracer (into [_tree] [(tree s)])))))

(defn tracer
  ([] (tracer [] #{}))
  ([tree] (tracer tree #{}))
  ([tree seen]
     (TraceSubstitutions. tree seen)))

(defn trace-subst? [x]
  (instance? TraceSubstitutions x))

(comment
  ;; works
  (tree (bind (bind (tracer) s#) s#))

  ;; works
  (tree (bind (tracer) (fresh [x] s# s#)))

  ;; works
  (tree (bind (tracer) (fresh [x] (fresh [y] s#) s#)))

  ;; works
  (tree
   (bind (tracer)
         (fresh [x]
           (conde
             [s# s#]
             [s# s#])
           s#)))

  (defn bar []
    (conde
      [s#]
      [s#]))

  (defn foo []
    (fresh [x y]
      s#
      (bar)))

  ;; can trace relations
  (tree (bind (tracer) (foo)))

  (defn aloop []
    (conde
      [s#]
      [(aloop)]))

  ;; works can handle recursive goals
  (tree (bind (tracer) (aloop)))

  ;; 1. we care about fresh
  ;; 2. we care about conde
  ;; 3. if encounter -inc, we force it
  ;; 4. we never call goals we've seen before
  ;; 5. we can test whether two goal are the same, their classes will match
  ;;    this works for regular relations, ones that don't using matching sugar
  )