(ns logic-fun.fast
  (:use [clojure.pprint :only [pprint]]))

;; Notes on how substitution works
;; 1) search until we find a value
;; 2) disallow addition substitutions that go back to the var we're looking for

(defn vec-last [v]
  (nth v (dec (count v))))

(defrecord lvarT [name])

(defn lvar? [x]
  (instance? lvarT x))

(defn lvar [name] (lvarT. name))

(defmethod print-method lvarT [x writer]
           (.write writer (str "<lvar:" (:name x) ">")))

;; NOTE: consider lowering to deftype
(defrecord pairT [lhs rhs])

(defn pair [lhs rhs]
  (pairT. lhs rhs))

(defprotocol ISubstitutions
  (length [this])
  (ext [this x v])
  (lookup [this v]))

(defn lookup* [ss v]
  (loop [v v a (vec-last (ss v)) ss ss orig v]
    (cond
     (nil? a)  v
     (= a orig) :circular
     (lvar? a) (recur a (vec-last (ss a)) ss orig)
     :else a)))

(defrecord Substitutions [ss order]
  ISubstitutions
  (ext [this x v]
       (if (= (lookup* ss x) :circular)
         nil
         (Substitutions. (update-in ss [x] (fnil conj []) v)
                         (conj order x))))
  (lookup [this v]
          (lookup* ss v)))

(defn empty-s []
  (Substitutions. {} []))

(comment
  ;; remember backwards for us since we're using vectors
  [[x 1] [y 5] [x y]]

  ;; prevent circular substs
  (let [x  (lvar 'x)
        y  (lvar 'y)
        ss (Substitutions. {x [y] y [x]} [x y])]
    (ext ss y y))
  
 ;; test
 (let [x  (lvar 'x)
       y  (lvar 'y)
       z  (lvar 'z)
       ss (Substitutions. {x [1 y]
                           y [5]}
                          [x y x])
       ss (ext ss x z)]
   (println ss))

 ;; not bad 500ms
 (dotimes [_ 10]
   (let [[x y z] [(lvar) (lvar) (lvar)]
         ss (Substitutions. {x [y 1] y [5]} [x y x])]
     (time
      (dotimes [_ 1e6]
        (ext ss x z)))))

 ;; ~300ms, pretty good, neet to compare to naive
 ;; for this degenerate case ~500ms
 (dotimes [_ 10]
   (let [[x y z] [(lvar) (lvar) (lvar)]
         ss (Substitutions. {x [y] y [z] z [5]} [x y z])]
     (time
      (dotimes [_ 1e6]
        (lookup ss x)))))

 ;; ~900ms
 ;; just a tiny bit faster than scheme
 (dotimes [_ 10]
   (let [[x y z c b a] [(lvar) (lvar) (lvar) (lvar) (lvar) (lvar)]
         ss (Substitutions. {x [5] y [x] z [y] c [z] b [c] a [b]} [x y z c b a])]
     (time
      (dotimes [_ 1e6]
        (lookup ss a)))))

 ;; degenerate case
 ;; same speed as above
 ;; Scheme is ~1130ms
 ;; so Scheme is doing okay, assq must be pretty quick
 (dotimes [_ 10]
   (let [[x m y n z o c p b q a] (take 11 (repeatedly lvar))
         ss (Substitutions. {x [5] y [x] z [y] c [z] b [c] a [b]} [x m y n z o c p b q a])]
     (time
      (dotimes [_ 1e6]
        (lookup ss a)))))

 (let [[x m y n z o c p b q a] (take 11 (repeatedly lvar))
         ss (Substitutions. {x [5] y [x] z [y] c [z] b [c] a [b]} [x m y n z o c p b q a])]
     (time
      (lookup ss a)))

 ;; erg before we get ahead of ourselves
 ;; preventing circularity
)

(comment

  ;; damn doesn't work
  (let [x []]
    (case (class x)
          clojure.lang.PersistentVector 'a
          clojure.lang.PersistentList 'b
          clojure.lang.PersistenHashMap 'c))

  (let [x 0]
    (case x
          0 'a
          1 'b
          2 'c))
)