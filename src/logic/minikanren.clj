(ns logic.minikanren
  (:refer-clojure :exclude [reify inc == take])
  (:use [clojure.pprint :only [pprint]]))

;; =============================================================================
;; Logic Variables

(deftype lvarT [name])

(defn ^lvarT lvar
  ([] (lvarT. (gensym)))
  ([name] (lvarT. name)))

(defmethod print-method lvarT [x writer]
           (.write writer (str "<lvar:" (.name ^lvarT x) ">")))

(deftype rest-lvarT [name])

(defn ^rest-lvarT rest-lvar
  ([] (rest-lvarT. (gensym)))
  ([name] (rest-lvarT. name)))

(defmethod print-method rest-lvarT [x writer]
           (.write writer (str "<lvar:" (.name ^lvarT x) ">")))

(defn lvar? [x]
  (or (instance? lvarT x) (instance? rest-lvarT x)))

;; TODO : why doesn't print-method get called during pretty printing ?

;; =============================================================================
;; Pairs

(defprotocol IPair
  (lhs [this])
  (rhs [this]))

(deftype pairT [lhs rhs]
  IPair
  (lhs [this] lhs)
  (rhs [this] rhs)
  clojure.lang.ISeq
  (first [this] lhs)
  (more [this] rhs))

(defn ^pairT pair [lhs rhs]
  (pairT. lhs rhs))

(defn pair? [x]
  (instance? pairT x))

(defmethod print-method pairT [x writer]
           (.write writer (str "(" (lhs x) " . " (rhs x) ")")))

;; =============================================================================
;; Unification

(declare lookup)
(declare lookup*)
(declare ext-no-check)
(declare ext)
(declare unify)
(declare length)
(declare empty-s)

;; TODO: unify* should fail in the 4th cond line if the types are not the same
;; lists should maybe unify with vectors

(defn print-identity [v]
  (println v) v)

(defn unify* [s u v]
  (let [u (lookup s u)
        v (lookup s v)]
    (cond
     (identical? u v) s
     (lvar? u) (if (lvar? v)
                 (ext-no-check s u v)
                 (ext s u v))
     (lvar? v) (ext s v u)
     (and (coll? u) (coll? v)) (let [[uf & ur] u
                                     [vf & vr] v
                                     s (unify s uf vf)]
                                 (and s (unify s ur vr)))
     (= u v) s
     :else false)))

;; =============================================================================
;; Reification

;; TODO: need to reconstruct the correct type

(defn reify-lookup [s v]
  (let [v (lookup s v)]
    (cond
     (lvar? v) v
     (coll? v) (cons (reify-lookup s (first v))
                     (reify-lookup s (next v)))
     :else v)))

(defn reify-name [s]
  (symbol (str "_." (length s))))

(defn -reify [s v]
  (let [v (lookup s v)]
    (cond
     (lvar? v) (ext s v (reify-name s))
     (coll? v) (-reify (-reify s (first v)) (next v))
     :else s)))

(defn reify [s v]
  (let [v (reify-lookup s v)]
    (reify-lookup (-reify empty-s v) v)))

;; =============================================================================
;; Substitutions

;; FIXME: there's a subtle bug here, v' might legitimately be nil, the
;; (vec-last (s v')), once I understand the goal part some more, we should
;; just maybe switch over to hash-maps

;; we can use find, returns the entry

(defn vec-last [v]
  (nth v (dec (count v))))

(defn lookup* [s v]
  (loop [v v v' (vec-last (s v)) s s ov v]
    (cond
     (nil? v')          v
     (identical? v' ov) ::circular
     (lvar? v')         (recur v' (vec-last (s v')) s ov)
     :else              v')))

(defprotocol ISubstitutions
  (length [this])
  (ext [this x v])
  (ext-no-check [this x v])
  (lookup [this v])
  (unify [this u v]))

;; NOTE: this data structure may be unecessary depending on how miniKanren actually works
;; because {} is already persistent, can probably just use it wholesale - David

;; one advantage of the current version is that we can see the order of the
;; substitutions, that's not possible with normal hash-map, useful for
;; debugging

(defrecord Substitutions [s order]
  ISubstitutions
  (length [this] (count order))
  (ext [this x v]
       (if (= (lookup* s x) ::circular)
         nil
         (ext-no-check this x v)))
  (ext-no-check [this x v]
                (Substitutions. (update-in s [x] (fnil conj []) v)
                                (conj order x)))
  (lookup [this v]
          (lookup* s v))
  (unify [this u v]
         (unify* this u v)))

(def empty-s (Substitutions. {} []))

(defn to-s [v]
  (let [s (reduce (fn [m [k v]]
                    (update-in m [k] (fnil conj []) v))
                  {} v)
        order (vec (map first v))]
    (Substitutions. s order)))

;; =============================================================================
;; Goals and Goal Constructors

(defmacro mzero [] false)

(defmacro unit [a] a)

(defmacro choice [a f]
  `(pair ~a ~f))

(defmacro inc [e]
  `(fn [] ~e))

(defn succeed [a]
  (unit a))

(defn fail [a]
  (mzero))

;; this is a conveniene binding macro for the different cases of a-inf
(defmacro case-inf [& [e _ e0 f' e1 a' e2 [a f] e3]]
  `(let [a-inf# ~e]
     (cond
      (not a-inf#) ~e0
      (fn? a-inf#) (let [~f' a-inf#] ~e1)
      (and (pair? a-inf#) (fn? (rhs a-inf#))) (let [~a (lhs a-inf#)
                                                    ~f (rhs a-inf#)]
                                                ~e3)
      :else (let [~a' a-inf#] ~e2))))

(defmacro == [u v]
  `(fn [a#]
     (if-let [s# (unify a# ~u ~v)]
       (unit s#)
       (mzero))))

;; a single value is just a single value
;; convert a sequence of values into a lazy stream
(defmacro mplus*
  ([e] e)
  ([e0 & e-rest] `(mplus ~e0 (fn [] (mplus* ~@e-rest)))))

(defn mplus [a-inf f]
  (case-inf a-inf
            false (f)
            f' (inc (mplus (f) f'))
            a (choice a f)
            [a f'] (choice a (fn [] (mplus (f) f')))))

(defn bind-cond-e-clause [s]
  (fn [[g0 & g-rest]]
    `(bind* (~g0 ~s) ~@g-rest)))

(defn bind-cond-e-clauses [s clauses]
  (map (bind-cond-e-clause s) clauses))

(defmacro cond-e [& clauses]
  (let [a (gensym "a")]
   `(fn [~a]
      (inc
       (mplus* ~@(bind-cond-e-clauses a clauses))))))

(defn lvar-binds [syms]
  (reduce concat (map (juxt identity (fn [s] `(lvar '~s))) syms)))

(defmacro exist [[& x-rest] g0 & g-rest]
  `(fn [a#]
     (inc
      (let [~@(lvar-binds x-rest)]
        (bind* (~g0 a#) ~@g-rest)))))

(defmacro bind*
  ([e] e)
  ([e g0 & g-rest] `(bind* (bind ~e ~g0) ~@g-rest)))

(defn bind [a-inf g]
  (case-inf a-inf
            false (mzero)
            f (inc (bind (f) g))
            a (g a)
            [a f] (mplus (g a) (fn [] (bind (f) g)))))

;; TODO: switch to conj onto a vector

(defmacro run [& [n [x] g0 & g-rest]]
  `(take ~n
         (fn []
           ((exist [~x] ~g0 ~@g-rest
                   (fn [a#]
                     (conj [] (reify a# ~x))))
            empty-s))))

(defn take [n f]
  (if (and n zero? n)
    []
    (case-inf (f)
              false []
              f (take n f)
              a a
              [a f] (cons (first a) (take (and n (dec n)))))))

(defmacro run* [& body]
  `(run false ~@body))

;; =============================================================================
;; Comments and Testing

(comment
 ;; ==================================================
 ;; TESTS

  (lvar 'x)

  (let [[x y z] (map lvar '[x y z])
        s (to-s [[x 5] [y x]])]
    s)

  (let [[x y z] (map lvar '[x y z])
        s (to-s [[x 5] [y x]])]
    (ext s z y))

  ;; nil
  (let [x (lvar 'x)
        y (lvar 'y)
        s (Substitutions. {x [y] y [x]} [x y])]
    (ext s y y))

  ;; 5
  (let [x  (lvar 'x)
        y  (lvar 'y)
        ss (to-s [[x 5] [y x]])]
    (lookup ss y)) ; 5

  ;; _.2
  (let [x  (lvar 'x)
        y  (lvar 'y)]
    (reify-name (to-s [[x 5] [y x]])))

  ;; (<lvar:x> <lvar:y>)
  (let [x  (lvar 'x)
        y  (lvar 'y)]
    (lookup (to-s [[x 5] [y x]]) `(~x ~y)))

  ;; 
  (let [x  (lvar 'x)
        y  (lvar 'y)]
    (-reify empty-s `(~x ~y)))

  ;; (5 5)
  (let [x  (lvar 'x)
        y  (lvar 'y)]
    (reify-lookup (to-s [[x 5] [y x]]) `(~x ~y)))

  ;; (5 _.0 (true _.1 _.0) _.2)
  (let [[x y z] (map lvar '[x y z])
        v `(5 ~x (true ~y ~x) ~z)
        r (reify empty-s v)]
    r)

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

  (dotimes [_ 10]
    (let [[x y z] (map lvar '[x y z])
          v `(5 ~x (true ~y ~x) ~z)
          r (reify empty-s v)]
      r))

  ;; ~1s, we're a bit ahead of Scheme at ~1.3s
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (run* [q]
             succeed
             (== true q)))))
 )

(comment
  
  (let [x (lvar 'x)
        y (lvar 'y)]
   (to-s [[x 5] [y x]]))

  (dotimes [_ 10]
    (let [p (pair 'a (fn []))]
     (time
      (dotimes [_ 1e8]
        (first p)
        (rest p)))))

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

;; Todos
;; 1) Understand the most basic case, ==
;; 2) Understand basic cond-e

;; Future Directions

;; 1) remove monadic style code, just use lazy-sequences
;; 2) more optimizations
;; 3) support Prolog style syntax, don't have to use exist implicit
;; 4) support using unify standalone, no need to call run
;; 5) investigate forward-chaining
;; 6) tabling
;; 7) consider parallel syntax
;; Datalog
