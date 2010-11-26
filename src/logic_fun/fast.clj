(ns logic-fun.fast
  (:refer-clojure :exclude [reify])
  (:use [clojure.pprint :only [pprint]]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Utilities

(defn vec-last [v]
  (nth v (dec (count v))))

;; =============================================================================
;; Logic Variables

(deftype lvarT [name])

(defn ^lvarT lvar
  ([] (lvarT. (gensym)))
  ([name] (lvarT. name)))

(deftype rest-lvarT [name])

(defn ^rest-lvarT rest-lvar
  ([] (rest-lvarT. (gensym)))
  ([name] (rest-lvarT. name)))

(defn lvar? [x]
  (or (instance? lvarT x) (instance? rest-lvarT x)))

(defmethod print-method lvarT [x writer]
           (.write writer (str "<lvar:" (.name ^lvarT x) ">")))

(defmethod print-method rest-lvarT [x writer]
           (.write writer (str "<lvar:" (.name ^lvarT x) ">")))

;; TODO : why doesn't print-method get called during pretty printing ?

;; =============================================================================
;; Pairs

(defprotocol IPair
  (lhs [this])
  (rhs [this]))

(defrecord pairT [lhs rhs]
  IPair
  (lhs [this] lhs)
  (rhs [this] rhs)
  ;; TODO : support destructuring
  ;; clojure.lang.Indexed
  ;; (nth [this idx] (case idx 0 lhs 1 rhs))
  ;; (nth [this idx default] (case idx 0 lhs 1 rhs :else default))
  )

(defn ^pairT pair [lhs rhs]
  (pairT. lhs rhs))

;; =============================================================================
;; Unification

(declare lookup)
(declare ext-no-check)
(declare ext)
(declare unify)
(declare length)
(declare empty-s)

(defn unify* [s u v]
  (let [u (lookup s u)
        v (lookup s v)]
    (cond
     (identical? u v) s
     (lvar? u) (if (lvar? v)
                 (ext-no-check s u v)
                 (ext s u v))
     (lvar? v) (ext s u v)
     (and (coll? u) (coll? v)) (let [[uf & ur] u
                                     [vf & vr] v
                                     s (unify s uf vf)]
                                 (and s (unify s ur vr)))
     (= u v) s
     :else false)))

;; =============================================================================
;; Reification

; TODO: needs to handle different types - David

(defn reify-lookup [s v]
  (let [v (lookup s v)]
    (cond
     (lvar? v) v
     (coll? v) (cons (reify-lookup s (first v))
                     (reify-lookup s (rest v)))
     :else v)))

(defn reify-name [s]
  (symbol (str "_." (length s))))

(defn -reify [s v]
  (let [v (lookup s v)]
    (cond
     (lvar? v) (ext s (reify-name s) v)
     (coll? v) (-reify (-reify s (first v))(rest v))
     :else s)))

(defn reify [s v]
  (let [v (reify-lookup s v)]
    (reify-lookup (-reify s (empty-s)) v)))

;; =============================================================================
;; Substitutions

(defprotocol ISubstitutions
  (length [this])
  (ext [this x v])
  (ext-no-check [this x v])
  (lookup [this v])
  (unify [this u v]))

(defrecord Substitutions [s order]
  ISubstitutions
  (length [this] (count order))
  (ext [this x v]
       (if (= (lookup* s x) :circular)
         nil
         (ext-no-check this x v)))
  (ext-no-check [this x v]
                (Substitutions. (update-in s [x] (fnil conj []) v)
                                (conj order x)))
  (lookup [this v]
          (lookup* s v))
  (unify [this u v]
         (unify* s u v)))

(defn ^Substitutions empty-s []
  (Substitutions. {} []))

;; =============================================================================
;; Goals and Goal Constructors

;; =============================================================================
;; Comments and Testing

(comment
 ;; ==================================================
 ;; TESTS

  ;; prevent circular substs
  (let [x  (lvar 'x)
        y  (lvar 'y)
        s (Substitutions. {x [y] y [x]} [x y])]
    (ext s y y))
  
  
 (let [x  (lvar 'x)
       y  (lvar 'y)
       z  (lvar 'z)
       ss (Substitutions. {x [1 y]
                           y [5]}
                          [x y x])]
   (ext ss x z))
 

 (let [x  (lvar 'x)
       y  (lvar 'y)
       ss (Substitutions. {x [1 y]
                           y [5]}
                          [x y x])]
   (lookup ss x))

 ;; ==================================================
 ;; PERFORMANCE
 
 ;; sick 470ms on 1.3.0 alph3
 (dotimes [_ 10]
   (let [[x y z :as s] (map lvar '[x y z])
         ss (Substitutions. {x [y 1] y [5]} s)]
     (time
      (dotimes [_ 1e6]
        (ext-no-check ss x z)))))

 ;; ~650ms
 (dotimes [_ 10]
   (let [[x y z :as s] (map lvar '[x y z])
         ss (Substitutions. {x [y 1] y [5]} s)]
     (time
      (dotimes [_ 1e6]
        (ext ss x z)))))

 ;; ~1200ms
 ;; just a tiny bit slower than the Scheme version
 (dotimes [_ 10]
   (let [[x y z c b a :as s] (map lvar '[x y z c b a])
         ss (Substitutions. {x [5] y [x] z [y] c [z] b [c] a [b]} s)]
     (time
      (dotimes [_ 1e6]
        (lookup ss a)))))

 ;; degenerate case
 (let [[x m y n z o c p b q a] (map lvar '[x m y n z o c p b q a])
       ss (Substitutions. {x [5] y [x] z [y] c [z] b [c] a [b]
                           m [0] n [1] o [2] p [3] q [4]}
                          [x m m m m y n n n n z o o o o
                           c p p p p b q q q q a])]
   (lookup ss a))

 ;; 600ms (NOTE: this jump is because array-map is slower than hash-maps)
 ;; Scheme is ~1650ms
 (dotimes [_ 10]
   (let [[x m y n z o c p b q a] (map lvar '[x m y n z o c p b q a])
         ss (Substitutions. {x [5] y [x] z [y] c [z] b [c] a [b]
                             m [0] n [1] o [2] p [3] q [4]}
                            [x m m m m y n n n n z o o o o
                             c p p p p b q q q q a])]
     (time
      (dotimes [_ 1e6]
        (lookup ss a)))))
 )

(comment
  (dotimes [_ 10]
    (let [v [1 2 3 4 5 6 7 8 9 0]]
     (time
      (dotimes [_ 1e6]
        (vec-last v)))))

  ;; interesting, destructuring is slow because we calling clojure.core/nth
  ;; instead of our own
  (let [p (pair 1 2)]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (let [[a b] p])))))

  ;; getting distract should look into what's going on here later
  (let* [p (pair 1 2)]
        (dotimes [_ 10]
          (time
           (dotimes [_ 1.0E7]
             (let* [vec__4455 p
                    a (nth vec__4455 0 nil)
                    b (nth vec__4455 1 nil)])))))

  (let [p [1 2]]
    (dotimes [_ 10]
      (time
       (dotimes [_ 1e7]
         (let [[a b] p])))))
)