(ns logos.scratch)

(deftype lvarT [name s]
  Object
  (toString [this] (str "<lvar:" name ">")))

;; =============================================================================
;; Rethinking how rest vars work, LCons

(defn ^lvarT lvar
  ([] (lvarT. (gensym) nil))
  ([name] (lvarT. name nil))
  ([name s] (lvarT. name s)))

(defn lvar? [x]
  (instance? lvarT x))

(defprotocol IPair
  (lhs [this])
  (rhs [this]))

(defprotocol LConsSeq
  (lfirst [this])
  (lnext [this])
  (lseq [this]))

(defprotocol LConsPrint
  (toShortString [this]))

;; LCons is NOT a dotted pair
;; it can only hold seqs or lvars in it's right hand side

(deftype LCons [a d]
  IPair
  (lhs [this] a)
  (rhs [this] d)
  LConsSeq
  (lfirst [_] a)
  (lnext [_] d)
  (lseq [this] this)
  LConsPrint
  (toShortString [this]
                 (cond
                  (lvar? d) (str a " . " d )
                  (instance? LCons d) (str a " " (toShortString d))))
  Object
  (toString [this] (cond
                    (lvar? d) (str "(" a " . " d ")")
                    (instance? LCons d) (str "(" a " " (toShortString d) ")"))))

(defmethod print-method LCons [x writer]
  (.write writer (str x)))

(defn lcons [a d]
  (LCons. a d))

(defn lcons? [x]
  (instance? LCons x))

(extend-type clojure.lang.PersistentVector
  LConsSeq
  (lfirst [this] (first this))
  (lnext [this] (next this))
  (lseq [this] (seq this)))

(extend-type clojure.lang.PersistentHashMap
  LConsSeq
  (lfirst [this] (first this))
  (lnext [this] (next this))
  (lseq [this] (seq this)))

(extend-type clojure.lang.PersistentArrayMap
  LConsSeq
  (lfirst [this] (first this))
  (lnext [this] (next this))
  (lseq [this] (seq this)))

(extend-type clojure.lang.PersistentList
  LConsSeq
  (lfirst [this] (first this))
  (lnext [this] (next this))
  (lseq [this] (seq this)))

(extend-type clojure.lang.PersistentHashSet
  LConsSeq
  (lfirst [this] (first this))
  (lnext [this] (next this))
  (lseq [this] (seq this)))

;; TODO: print method
;; TODO: investigate sequence printing

;; =============================================================================

(defprotocol ICondT
  (aPlusB [this ^int a ^int b]))

(deftype CondT []
  ICondT
  (aPlusB [this a b] (+ a b)))

(defn choice [a b]
  (cond
   (instance? CondT a) false
   (instance? CondT b) false
   (fn? a) false
   :else (+ a b)))

(defprotocol IReify
  (reify-lookup [this s]))

(extend-type clojure.lang.IPersistentVector
  IReify
  (reify-lookup [this s] s))

(comment

(dotimes [_ 10]
  (let [x 1]
   (time
    (dotimes [_ 1e8]
      (let [m  (CondT.)]
        (.aPlusB m 1 2)
        (.aPlusB m 1 2))))))

(dotimes [_ 10]
  (let [x 1]
   (time
    (dotimes [_ 1e8]
      (choice 1 2)))))

 (dotimes [_ 10]
   (let [v []
         x 1]
     (time
      (dotimes [_ 1e8]
        (reify-lookup v 1)))))

 ;; < 1 s w/o type check
 ;; ~1.8s to get the value out of the map
 ;; < 2 s w/ type check
 (dotimes [_ 10]
   (let [v (with-meta [1] {:type ::foo})]
     (time
      (dotimes [_ 1e8]
        (:type (meta v))))))

 ;; ~550ms
 (dotimes [_ 10]
   (let [v [1]]
     (time
      (dotimes [_ 1e8]
        (nth v 0)))))
 )