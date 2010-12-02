(ns logos.scratch)

(deftype Unit [a]
  clojure.lang.IFn
  (invoke [this] a))

(defn unit [a]
  (Unit. a))

;; 700ms
;; which is really good
(dotimes [_ 10]
  (time
   (dotimes [_ 1e8]
     (unit 'a))))

;; ~130ms
(dotimes [_ 10]
  (let [a-inf (unit 'a)]
   (time
    (dotimes [_ 1e8]
      (a-inf)))))

;; the same
(dotimes [_ 10]
  (time
   (dotimes [_ 1e8]
     'a)))

;; Construction not access seems like the primary cost

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