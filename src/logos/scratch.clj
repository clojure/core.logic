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

(deftype LCons [a d]
  IPair
  (lhs [this] a)
  (rhs [this] d)
  clojure.lang.ISeq
  (first [_] a)
  #_(more [_] d)
  (next [_] (cond
             (seq? d) d))
  (seq [this] this)
  (equiv [this that] false)
  Object
  (toString [this] (if (lvar? d)
                     (str "(" a " . " d ")")
                     this)))

(defn lcons [a d]
  (LCons. a d))

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