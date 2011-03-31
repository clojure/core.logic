(ns logos.disequality
  (:refer-clojure :exclude [reify == inc])
  (:use [logos.minikanren :exclude [==]]
        [clojure.set :only [rename-keys]]
        logos.match)
  (:import [logos.minikanren Substitutions Pair]))

;; we should really consider putting complex constraints into the Substitution
;; when a var gets unified with a var it should copy over the simple constraints

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
(defn ^Substitutions constraint-verify [s u v l verify cs]
  (if-let [uc (constraints u)]
    (if (contains? uc v)
      nil
      (let [u (remove-constraints u)]
        (if (lvar? v)
          (let [v (add-constraints v uc)]
            (make-s (assoc s u v) (cons (pair u v) l) verify cs))
          (make-s (assoc s u v) (cons (pair u v) l) verify cs))))
    (make-s (assoc s u v) (cons (pair u v) l) verify cs)))

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

;; ah we need to check we didn't violate anything
(comment
 (defn constrain [s u c]
   (let [u' (walk u)]
     (if (lvar? u')
       (let [c (merge-constraints (meta u') c)]
         (swap s (with-meta u' c)))
       (if (contains?)))))
 ) 

(defn all-different [& lvars]
  (let [c (set lvars)]
    (fn [a]
      (let [clvars (map (fn [lvar]
                          (with-meta lvar
                            {:simple (disj c lvar)
                             :complex #{}}))
                        lvars)]
        (apply unbound* a clvars)))))

(comment
  (let [[x y z] (map lvar '(x y z))]
    (map meta
         (-> empty-s
             ((all-different x y z))
             .s
             keys)))

  ;; 500ms
  (dotimes [_ 5]
    (let [[x y z] (map lvar '(x y z))]
     (time
      (dotimes [_ 1e5]
        (map meta
             (-> empty-s
                 ((all-different x y z))
                 .s
                 keys))))))

  (let [x (lvar 'x)
        y (lvar 'y)
        z (lvar 'z)
        s1 (-> empty-s
                (ext-no-check x 1)
                (ext-no-check y 2))
        s2 (ext-no-check s1 z 3)]
    (prefix (.l s2) (.l s1)))

  ;; plenty fast
  ;; 1s
  (dotimes [_ 10]
    (time
     (dotimes [_ 2e6]
       (let [x (lvar 'x)
             y (lvar 'y)
             z (lvar 'z)
             s1 (-> empty-s
                    (ext-no-check x 1)
                    (ext-no-check y 2))
             s2 (ext-no-check s1 z 3)]
         (prefix (.l ^Substitutions s2) (.l ^Substitutions s1))))))

  (merge-constraints
   {:simple #{:a :b :c} :complex #{{:a 1 :b 2}}}
   {:simple #{:d :e} :complex #{{:d 3 :e 4}}})

  ;; 1.6s
  ;; pretty fast considering
  (dotimes [_ 10]
    (let [c1 {:simple #{:a :b :c} :complex #{{:a 1 :b 2}}}
          c2 {:simple #{:d :e} :complex #{{:d 3 :e 4}}}]
     (time
      (dotimes [_ 1e6]
        (merge-constraints c1 c2)))))

  ;; about the same amount of time
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (merge-constraints
        {:simple #{:a :b :c} :complex #{{:a 1 :b 2}}}
        {:simple #{:d :e} :complex #{{:d 3 :e 4}}}))))

  (let [x (lvar 'x)
        y (lvar 'y)
        z (lvar 'z)]
    (-> empty-s
        (unify x [y z])
        (unify y 1)
        .s))
  )