(ns logos.core)

(deftype lvarT [])

(defn lvar? [x]
  (instance? lvarT x ))

(defn lvar [] (lvarT.))

(def empty-s '())

(defn ext-s-no-check [x v s]
  (cons [x v] s))

(defn ext-s [])

(defn walk [v s]
  (cond
   (lvar? v) (let [[_ rhs :as a]
                   (some #(when (= v (first %)) %) s)]
               (cond
                a (walk rhs s)
                :else v))
   :else v))

;; changes

(comment
  ;; making a lot of maps is pretty quick
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       {'foo 'bar})))

  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       ['foo 'bar])))

  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (rand-int 10))))

  ;; constructing sets is much slower
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       #{'foo 'bar})))

  ;; not bad
  (dotimes [_ 10]
    (let [foo {'x '(y 1)
               'y '(5)}]
     (time
      (dotimes [_ 1e6]
        (update-in foo ['x] conj 2)))))

  ;; transactions make it way more expensive
  (dotimes [_ 10]
    (let [foo (ref {'x '(y 1)
                    'y '(5)})]
     (time
      (dotimes [_ 1e6]
        (dosync
         (alter foo update-in ['x] conj 2))))))
  )
