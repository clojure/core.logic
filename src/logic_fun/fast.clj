(ns logic-fun.fast)

(defprotocol ISubstitutions
  (length [this])
  (ext [this s])
  (pop [this s]))

(defrecord Substitutions [ss order]
  ISubstitutions
  (length [this] (count order))
  (ext [this [a b :as s]]
       (Substitutions. (update-in ss [a] (fnil conj []) b)
                       (conj order a))))

(comment
 ;; test
 (let [ss (Substitutions. {'x '[y 1]
                           'y '[5]}
                          '[x y x])
       ss (ext ss ['z 'y])]
   (println ss)
   (println (length ss)))

 ;; perf
 (dotimes [_ 10]
   (let [ss (Substitutions. {'x '(y 1)
                            'y '(5)}
                           ('x 'y 'x))]
     (time
      (dotimes [_ 1e6]
        (push ss ['z 'y])))))
)

(deftype lvar [v])

(defn lvar? [x]
  (instance? lvar x ))

;; we could store all vars in a map
(comment
  {'x [y]
   'y [5 1]}
  )

;; but what happens when we pop a substitution off ?
(comment
  ;; represents the above
  [[x y] [5 y] [y 1]]

  ;; we could construct the map each time, probably expensive
  ;; as declaring logic vars is quite common

  ;; but what would a better data structure look like
  ;; that allows us to preserve the backtracking component ?

  ;; we could keep a list of the current substitutions
  {'x [y]
   'y [5 1]}

  ;; subst-order
  ['x 'y 'y]

  ;; do we need to make a custom data structure for this?

  ;; we could hide it inside a datatype ?
  ;; since methods are already synchronized and they are opaque
  )

;; one holds the order
(def subst-order (ref '()))
o;; the other holds the mappings
(def substs (ref '{}))

(def empty-s [])

;; we could keep association lists for each var, refs
;; then we wouldn't have to walk the whole shebang

(defn walk [v s]
  (cond
   (lvar? v) (let [[_ rhs :as a] (some #(= v (first %)) s)]
               (cond
                a (walk rhs s)
                :else v))
   :else v))