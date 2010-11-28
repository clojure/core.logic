(ns logic.minikanren
  (:refer-clojure :exclude [reify inc == take])
  (:use [clojure.pprint :only [pprint]]
        [clojure.walk :only [postwalk]]))

;; =============================================================================
;; Logic Variables

(deftype lvarT [name s]
  Object
  (toString [this] (str "<lvar:" name ">")))

(defn ^lvarT lvar
  ([] (lvarT. (gensym) nil))
  ([name] (lvarT. name nil))
  ([name s] (lvarT. name s)))

(defmethod print-method lvarT [x writer]
  (.write writer (str "<lvar:" (.name ^lvarT x) ">")))

(deftype rest-lvarT [name s]
  clojure.lang.Seqable
  (seq [this] (list this)))

(defn ^rest-lvarT rest-lvar
  ([] (rest-lvarT. (gensym) nil))
  ([name] (rest-lvarT. name nil))
  ([name s] (rest-lvarT. name s)))

(defmethod print-method rest-lvarT [x writer]
  (.write writer (str "<lvar:" (.name ^rest-lvarT x) ">")))

(defn lvar? [x]
  (or (instance? lvarT x) (instance? rest-lvarT x)))

(defn rest-lvar? [x]
  (instance? rest-lvarT x))

(defn rest-lvar-sym? [x]
  (= (first (str x)) \&))

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
  (more [this] rhs)
  Object
  (toString [this] (str "(" lhs " . " rhs ")")))

(defn ^pairT pair [lhs rhs]
  (pairT. lhs rhs))

(defn pair? [x]
  (instance? pairT x))

(defmethod print-method pairT [x w]
  (.write w (str "(" (lhs x)  " . " (rhs x)  ")")))

;; =============================================================================
;; Unification

(declare lookup)
(declare lookup*)
(declare ext-no-check)
(declare ext)
(declare unify)
(declare length)
(declare empty-s)

;; NOTE: should unify* fail in the 4th cond line if the coll types are not the
;; same?

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
     (and (coll? u) (coll? v)) (let [[uf & ur] (seq u)
                                     [vf & vr] (seq v)]
                                 (cond
                                  (rest-lvar? uf) (unify s uf v)
                                  (rest-lvar? vf) (unify s vf u)
                                  :else (let [s (unify s uf vf)]
                                          (and s (unify s ur vr)))))
     (= u v) s
     :else false)))

;; =============================================================================
;; Reification

(defn reify-lookup [s v]
  (let [v (lookup s v)]
    (cond
     (lvar? v) v
     (coll? v) (let [vseq (if (map? v) (reduce concat v) v)
                     r (cons (reify-lookup s (first vseq))
                             (reify-lookup s (next vseq)))]
                 (cond
                  (vector? v) (vec r)
                  (map? v) (apply hash-map r)
                  (set? v) (set r)
                  :else r))
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

(defn lookup* [s v]
  (loop [v v p (find s v) s s ov v]
    (if (nil? p)
      v
      (let [[v v'] p]
        (if (lvar? v')
          (recur v' (find s v') s ov)
          v')))))

(defprotocol ISubstitutions
  (length [this])
  (ext [this x v])
  (ext-no-check [this x v])
  (lookup [this v])
  (unify [this u v]))

(defrecord Substitutions [s s']
  ISubstitutions
  (length [this] (count s'))
  (ext [this x v]
       (if (= (lookup* s x) ::circular)
         nil
         (ext-no-check this x v)))
  (ext-no-check [this x v]
                (Substitutions. (assoc s x v)
                                (conj s' (pair x v))))
  (lookup [this v]
          (lookup* s v))
  (unify [this u v]
         (unify* this u v)))

(def empty-s (Substitutions. {} []))

(defn to-s [v]
  (let [s (reduce (fn [m [k v]] (assoc m k v)) {} v)
        s' (vec (map (partial apply pair) v))]
    (Substitutions. s s')))

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

(def s# succeed)

(def u# fail)

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

(defn lvar-bind [sym]
  ((juxt identity
         (fn [s] (if (rest-lvar-sym? s)
                   `(rest-lvar '~s)
                   `(lvar '~s)))) sym))

(defn lvar-binds [syms]
  (reduce concat (map lvar-bind syms)))

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

;; TODO: find for what reason are putting the value in a vector?

(defmacro run [& [n [x] g0 & g-rest]]
  `(take ~n
         (fn []
           ((exist [~x] ~g0 ~@g-rest
                   (fn [a#]
                     (conj [] (reify a# ~x))))
            empty-s))))

(defn take
  ([n f] (take n f [])) 
  ([n f v]
     (if (and n zero? n)
       v
       (case-inf (f)
                 false v
                 f (take n f v)
                 a (conj v (first a))
                 [a f] (take (and n (dec n)) f (conj v (first a)))))))

(defmacro run* [& body]
  `(run false ~@body))

;; =============================================================================
;; Unifier

(defn lvar-sym? [s]
  (= (first (str s)) \?))

(defn rem-? [s]
  (symbol (apply str (drop 1 (str s)))))

(defn replace-lvar [expr]
  (if (lvar-sym? expr)
    (lvar (rem-? expr))
    expr))

(defn prep [expr]
  (postwalk replace-lvar expr))

(defn unifier [u w]
  (run* [q]
        (== u w)
        (== u q)))

(defn unifier' [u w]
  (let [u' (prep u)
        w' (prep w)]
   (run* [q]
         (== u' w')
         (== u' q))))

;; =============================================================================
;; Core functions

(defn nil-o [a]
  (== nil a))

(defn cons-o [a l]
  (exist [c &d]
         (== (cons a &d) l)))

(defn rest-o [l d]
  (exist [a]
         (== (cons a d) l)))

(defn first-o [a l]
  (cons-o a l))

(defn pair-o [p]
  ())

(defn twin-o [p]
  )

(defn append-o [a b]
  )

;; =============================================================================
;; Logic REPL

(defn not-quit? [s]
  (let [e (read-string s)]
   (if (= e '(quit))
     ::exit
     (eval e))))

(defn repl []
  (.print System/out "?- ")
  (.flush System/out)
  (let [v (not-quit? (read-line))]
    (when (not= v ::exit) 
      (println v)
      (recur))))

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

  ;; 5
  (let [x  (lvar 'x)
        y  (lvar 'y)
        ss (to-s [[x 5] [y x]])]
    (lookup ss y))

  ;; 5
  (let [[x y z c b a :as s] (map lvar '[x y z c b a])
        ss (to-s [[x 5] [y x] [z y] [c z] [b c] [a b]])]
    (lookup ss a))
  
  ;; 5, degenerate case
  (let [[x m y n z o c p b q a] (map lvar '[x m y n z o c p b q a])
        ss (to-s [[x 5] [m 0] [m 0] [m 0] [m 0]
                  [y x] [n 0] [n 0] [n 0] [n 0]
                  [z y] [o 0] [o 0] [o 0] [o 0]
                  [c z] [p 0] [p 0] [p 0] [p 0]
                  [b c] [q 0] [q 0] [q 0] [q 0]
                  [a b]])]
    (lookup ss a))

  ;; _.2
  (let [x  (lvar 'x)
        y  (lvar 'y)]
    (reify-name (to-s [[x 5] [y x]])))

  ;; (<lvar:x> <lvar:y>)
  (let [x  (lvar 'x)
        y  (lvar 'y)]
    (lookup (to-s [[x 5] [y x]]) `(~x ~y)))

  ;; (5 5)
  (let [x  (lvar 'x)
        y  (lvar 'y)]
    (reify-lookup (to-s [[x 5] [y x]]) `(~x ~y)))

  ;; (5 _.0 (true _.1 _.0) _.2)
  (let [[x y z] (map lvar '[x y z])
        v `(5 ~x (true ~y ~x) ~z)
        r (reify empty-s v)]
    r)

  (run* [q]
        (== true q))

  (run* [q]
        succeed
        (== true q))

  (run* [q]
        fail
        (== true q))

  (run* [q]
        (== false q)
        (== true q))

  ;; [[1 5]]
  (run* [q]
        (exist [x y]
               (== [x y] [1 5])
               (== [x y] q)))

  ;; [[1 5]]
  (run* [q]
        (exist [x y]
               (== {x y} {1 5})
               (== [x y] q)))

  ;; [[_.0 _.1]]
  (run* [q]
        (exist [x y]
               (== [x y] q)))

  (run* [q]
        (exist [x y]
               (== {x y} q)))

  (run* [q]
        (exist [x y z]
               (== y z)
               (== [1 2 {x y}] q)
               (== z 5)))

  (let [[x y q] (map lvar '[x y q])
        s (unify empty-s {(lvar 'x) (lvar 'y)} q)]
    (reify s q))

  (run* [q]
        (exist [x y r]
         (== {x y} r)
         (== {x y} {1 5})
         (== r q)))

  (run* [q]
        (exist [x y z]
               (== y z)
               (== [1 2 {x y}] q)
               (== z 5)))

  (run* [q]
        (exist [x y z]
               (== y z)
               (== [1 2 #{x y}] q)
               (== z 5)))

  (run* [x]
        (cond-e
         ((== x 'olive) succeed)
         (succeed succeed)
         ((== x 'oil) succeed)))

  (run* [r]
        (exist [x y]
               (cond-e
                ((== 'split x) (== 'pea y))
                ((== 'navy x) (== 'bean y)))
               (== (cons x (cons y ())) r)))

  (defn teacup-o [x]
    (cond-e
     ((== 'tea x) s#)
     ((== 'cup x) s#)))

  ;; the return order is interesting
  ;; interleaving
  (run* [r]
        (exist [x y]
               (cond-e
                ((teacup-o x) (== true y) s#)
                ((== false x) (== true y)))
               (== (cons x (cons y ())) r)))

  (let [x (lvar 'x)
        y (lvar 'y)
        v [x y]]
    (run* [q]
          (== v q)))

  ;; attempt to understand the above
  (take
   false
   (fn []
     ((fn [a__10796__auto__]
        (fn []
          (let [x (lvar 'x)]
            (bind
             ((fn [a10883]
                (fn []
                  (mplus
                   (bind ((fn [a__10763__auto__]
                            (if-let [s__10764__auto__ (unify a__10763__auto__ x 'olive)]
                              s__10764__auto__
                              false)) a10883) succeed)
                   (fn [] (bind ((fn [a__10763__auto__]
                                   (if-let [s__10764__auto__ (unify a__10763__auto__ x 'oil)]
                                     s__10764__auto__
                                     false)) a10883) succeed)))))
              a__10796__auto__)
             (fn [a__10816__auto__] (conj [] (reify a__10816__auto__ x)))))))
      empty-s)))

  (run* [q]
        (cons-o 'a q))

  (run* [q]
        (exist [a]
               (cons-o a q)))

  (run* [q]
        (first-o q [1 2]))

  (run* [q]
        (rest-o q [1 2]))

  ;; FIXME: doesn't work

  (run* [&q]
        (rest-o [1 2] &q))

  (run* [&q]
        (rest-o [1 2 3 4 5 6 7 8] &q))

  ;; tricky need to
  (unifier '(?x ?y) '(1 2))

  ;; ==================================================
  ;; PERFORMANCE
  
  ;; sick 183ms on 1.3.0 alph3
  (dotimes [_ 10]
    (let [[x y z :as s] (map lvar '[x y z])
          ss (to-s [[x 5] [y x]])]
      (time
       (dotimes [_ 1e6]
         (ext-no-check ss z y)))))

  ;; ~239ms
  (dotimes [_ 10]
    (let [[x y z :as s] (map lvar '[x y z])
          ss (to-s [[x 5] [y x]])]
      (time
       (dotimes [_ 1e6]
         (ext ss x z)))))

  ;; ~700ms
  ;; just a tiny bit slower than the Scheme version
  (dotimes [_ 10]
    (let [[x y z c b a :as s] (map lvar '[x y z c b a])
          ss (to-s [[x 5] [y x] [z y] [c z] [b c] [a b]])]
      (time
       (dotimes [_ 1e6]
         (lookup ss a)))))

  ;; 200ms (NOTE: this jump is because array-map is slower than hash-maps)
  ;; Scheme is ~1650ms
  (dotimes [_ 10]
    (let [[x m y n z o c p b q a] (map lvar '[x m y n z o c p b q a])
          ss (to-s [[x 5] [m 0] [m 0] [m 0] [m 0]
                    [y x] [n 0] [n 0] [n 0] [n 0]
                    [z y] [o 0] [o 0] [o 0] [o 0]
                    [c z] [p 0] [p 0] [p 0] [p 0]
                    [b c] [q 0] [q 0] [q 0] [q 0]
                    [a b]])]
      (time
       (dotimes [_ 1e6]
         (lookup ss a)))))

  ;; 1.5 million unifications a second, not bad
  (dotimes [_ 10]
    (time
     (dotimes [_ 1.5e6]
       (run* [q]
             (== true q)))))
  
  ;; ~560ms!!!
  ;; Scheme at ~1.3s
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (run* [q]
             succeed
             (== true q)))))

  ;; 1s for 1e5
  ;; 2s for Scheme, so Clojure miniKanren is about twice as fast
  (dotimes [_ 10]
   (time
    (dotimes [_ 1e5]
     (run* [r]
           (exist [x y]
                  (cond-e
                   ((teacup-o x) (== true y) s#)
                   ((== false x) (== true y)))
                  (== (cons x (cons y ())) r))))))

  ;; 200,000 unifications
  ;; what kind of boost could we get with tabling ?
  (dotimes [_ 10]
    (time
     (dotimes [_ 2e5]
       (run* [q]
             (exist [x y]
                    (== [x y] [1 5])
                    (== [x y] q))))))

  ;; 2.5s, much slower
  (dotimes [_ 10]
    (time
     (dotimes [_ 2e5]
       (run* [q]
             (exist [x y z]
                    (== y z)
                    (== [1 2 {x z}] q)
                    (== z 5))))))

  ;; 1.8s, if we had something eliminated the needless unifications
  ;; not that much of an improvement for 2e5
  (dotimes [_ 10]
    (time
     (dotimes [_ 2e5]
       (run* [q]
             (exist [x y z]
                    (== [1 2 {x 5}] q))))))

  ;; this is going to be very, very slow
  ;; postwalk is not cheap
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e4]
       (unifier '{?x ?y} {1 2}))))

  (dotimes [_ 10]
    (let [[u w] (map prep ['{?x ?y} {1 2}])]
     (time
      (dotimes [_ 1e5]
        (unifier u w)))))
 )

(comment
  
  (let [x (lvar 'x)
        y (lvar 'y)]
   (to-s [[x 5] [y x]]))

  ;; TODO : does not work, rest tries to convert the remainder into a seq
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
;; 1) Understand basic cond-e

;; Future Directions

;; 1) compare with lazy sequences instead of (a . f)
;; 2) more optimizations
;; 3) support Prolog style syntax, don't have to use exist implicit
;; 4) support using unify standalone, no need to call run
;; 5) investigate forward-chaining
;; 6) tabling
;; 7) consider parallel syntax
;; Datalog
