(ns logos.disequality
  (:refer-clojure :exclude [reify == inc])
  (:use [logos.minikanren :exclude [==]]
        [clojure.set :only [rename-keys]]
        logos.match)
  (:import [logos.minikanren Substitutions Pair]))

(defn prefix [s <s]
  (if (= s <s)
    ()
    (cons (first s) (prefix (rest s) <s))))

(defn unify* [^Substitutions s c]
  (loop [[[u v :as b] & cr] c nc #{}]
    (let [^Substitutions s' (unify s u v)]
      (cond
       (nil? b) (if (seq nc) nc false) ;; are we done?
       (or (identical? s s') (not s')) (recur cr nc) ;; violated sub-constraint or a discard
       :else (recur cr (conj nc (prefix (.l s') (.l s))))))))

;; SIMPLE
;; if u has no constraints just extend s
;; if u's constraints contain v, fail
;; if v is an lvar move u's constraints over to support tri subst
;; if v is not an lvar, we can discard the constraints
;; COMPLEX
;; look up u in cs and see what we can do
(defn ^Substitutions constraint-verify [s u v l verify cs]
  (let [uc (constraints u)]
    (if (contains? uc v)
      nil
      (let [u (remove-constraints u)
            v (if (lvar? v) (add-constraints v uc) v)]
        (make-s (-> s (dissoc u) (assoc u v)) (cons (pair u v) l) verify cs)))))

(defprotocol IDisequality
  (!=-verify [this sp]))

(extend-type Substitutions
  IDisequality
  (!=-verify [this sp]
             (let [^Substitutions sp sp]
              (cond
               (not sp) this
               (= this sp) nil
               :else (let [[[u v] :as c] (prefix (.l sp) (.l this))
                           simple (= (count c) 1)]
                       (if simple
                         (let [u (walk this u)
                               v (walk this v)]
                           (cond
                            (= u v) nil
                            (lvar? v) (let [uc (constraints u)
                                            u (remove-constraints u)
                                            v (add-constraints v uc)]
                                        (-> this (swap u) (swap v))) ;; WRONG
                            :else this)))))))) ;; WRONG

(defmacro != [u v]
  `(fn [a#]
     (!=-verify a# (unify a# u v))))

(comment
  ;; NOTE: tri subst preserve never setting a var twice

  ;; should fail
  (let [[x y] (map lvar '[x y z])
        x (add-constraint x 1)]
    (constraint-verify {x y} x 1 (cons (pair x y) nil) constraint-verify nil))

  ;; should move simple constraints
  (let [[x y z] (map lvar '[x y z])
        y (add-constraint y 1)
        ns (constraint-verify {x y} y z (cons (pair x y) nil) constraint-verify nil)]
    (constraints ((.s ns) y)))

  ;; nil
  (let [[x y z] (map lvar '[x y z])
        y (add-constraint y 1)
        ns (constraint-verify {x y} y 2 (cons (pair x y) nil) constraint-verify nil)]
    (constraints (first (keys (.s ns)))))

  (let [m {}
        y (lvar 'y)
        yc (add-constraint y 1)]
    (-> m
        (assoc y 2)
        (assoc yc 3)
        keys
        first
        constraints)) ;; keys are not replaced if already equal!

  ;; big problem, lexical scoping, how can we add constraints w/o mutation?
  ;; will refer to the var w/o constraints

  (exist [y]
         (!= y 1)
         (foo y z) ;; we passing the y w/o 
         (== y 1))

  ;; we just set a flag on the var, the var is constrained
  ;; looking for simplified constraints in the constraint will be much slower than
  ;; accessing meta

  ;; the order of constraints doesn't matter w/in an exist clause

  ;; actually not true at all
  (let [m {:foo 'bar 1 2 3 4 5 6 7 8 9 0 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e6]
         (m :foo)))))

  (let [m {:foo 'bar}]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e8]
         (:foo m)))))

  ;; we can move the constraints into the declaration of the var!
 ) 
