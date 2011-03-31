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
        (make-s (assoc s u v) (cons (pair u v) l) verify cs)))))

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
                                        (-> this
                                            (swap u)
                                            (swap v)))
                            :else this))))))))

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

  (let [[x y z] (map lvar '[x y z])
        x (add-constraint y 1)
        ns (constraint-verify {x y} y 2 (cons (pair x y) nil) constraint-verify nil)]
    (.s ns))
 ) 
