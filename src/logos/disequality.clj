(ns logos.disequality
  (:refer-clojure :exclude [reify == inc])
  (:use [logos.minikanren :exclude [==]]
        [clojure.set :only [rename-keys]]
        logos.match)
  (:import [logos.minikanren Substitutions Pair]))

;; all-different ?

(defmacro != [u v]
  `(fn [a#]
     (!=-verify a# (unify a# u v))))

(defn verify-simple [s v c]
  (not (contains? c v)))

(defn simplify [s complex]
  )

(declare prefix)

(defn seive [c*]
  (group-by (fn [x] (if (vector? x)
                      :complex
                      :simple))
            c*))

(defn unify* [^Substitutions s c]
  (loop [[[u v :as b] & cr] c nc []]
    (let [^Substitutions s' (unify s u v)]
      (cond
       (nil? b) (if (seq nc) nc false)
       (identical? s s') (recur cr nc)
       (not s') (recur cr (conj nc b))
       :else (recur cr (conj nc (prefix (.l s') (.l s))))))))

(comment
  (let [x (lvar 'x)
        y (lvar 'y)
        z (lvar 'z)
        s (-> empty-s
              (ext-no-check x 1)
              (ext-no-check z 3))]
    (unify* s [(Pair. y 2) (Pair. x 1)]))
  )

(declare merge-contraints)

(defn verify-complex [s u c*]
  (loop [[c & cr :as c*] c*]
    (let [nc (unify* s c*)]
      (cond
       (nil? cr) (merge-contraints (meta u) (seive c*))
       :else nil))))

;; by this point, unification succeeded
;; we need to return substitution, because we want to return
;; the substitution w/ updated constraints
;; some complexity since (Pair. u v) isn't already in the substitution
(defn constraint [s u v]
  (if-let [meta (meta u)]
    (let [{:keys [simple complex]} meta]
      (let [[valid new-complex] (verify-complex s u v complex)]
        (if (not valid)
          nil
          (if (verify-simple s v simple)
            s))))
    s))

(defn merge-constraints [c1 c2]
  (-> c1
      (update-in [:simple] #(reduce conj % (:simple c2)))
      (update-in [:complex] #(reduce conj % (:complex c2)))))

(defn prefix [s <s]
  (if (= s <s)
    ()
    (cons (first s) (prefix (rest s) <s))))

(defprotocol IDisequality
  (!=-verify [this sp])
  (==-verify [this u v]))

(extend-type Substitutions
  IDisequality

  (!=-verify [this sp]
             (let [^Substitutions sp sp]
              (cond
               (not sp) this
               (= this sp) nil
               :else (let [[[k v] & r :as c] (into {} (prefix (.l sp) (.l this)))
                           nc (if (= (count c) 1)
                                {:simple #{v} :complex []}
                                {:simple #{} :complex [c]})
                           ks (keys c)
                           nks (zipmap ks (map #(with-meta %
                                                  (merge-constraints (meta %) nc))
                                               ks))
                           os (.s this)]
                       (make-s (rename-keys os nks)
                               (.l this) constraint))))))

(comment
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
   {:simple #{:a :b :c} :complex [{:a 1 :b 2}]}
   {:simple #{:d :e} :complex [{:d 3 :e 4}]})

  ;; 1.6s
  ;; pretty fast considering
  (dotimes [_ 10]
    (let [c1 {:simple #{:a :b :c} :complex [{:a 1 :b 2}]}
          c2 {:simple #{:d :e} :complex [{:d 3 :e 4}]}]
     (time
      (dotimes [_ 1e6]
        (merge-constraints c1 c2)))))

  ;; about the same amount of time
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (merge-constraints
        {:simple #{:a :b :c} :complex [{:a 1 :b 2}]}
        {:simple #{:d :e} :complex [{:d 3 :e 4}]}))))
  )