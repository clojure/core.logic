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

(defn prefix [s <s]
  (if (= s <s)
    ()
    (cons (first s) (prefix (rest s) <s))))

(defn merge-constraints [c1 c2]
  (-> c1
      (update-in [:simple] #(reduce conj % (:simple c2)))
      (update-in [:complex] #(reduce conj % (:complex c2)))))

(defn unify* [^Substitutions s c]
  (loop [[[u v :as b] & cr] c nc #{}]
    (let [^Substitutions s' (unify s u v)]
      (cond
       (nil? b) (if (seq nc) nc false) ;; are we done?
       (or (identical? s s') (not s')) (recur cr nc) ;; violated sub-constraint or a discard
       :else (recur cr (conj nc (prefix (.l s') (.l s)))))))) ;; 

(defn verify-complex [s u c*]
  (loop [[c & cr :as c*] c* s* #{} nc* #{}]
    (let [nc (unify* s c)
          j (count nc)]
      (cond
       (nil? cr) (merge-constraints (meta u) {:simple s* :complex nc*})
       (zero? j) false
       (= j 1) (let [[u' v :as p] nc]
                 (if (= u' u)
                  (recur cr (conj s* p) nc*)
                  (recur cr s* nc*)))
       :else (recur cr s* (conj nc* nc))))))

;; nil, new constraint, ::violated
;; we would prefer to return one kind of value
(defn constraint [s u v]
  (if-let [meta (meta u)]
    (let [{:keys [simple complex]} meta]
      (if (verify-simple s v simple)
        (if (seq complex)
         (if-let [nc* (verify-complex u complex)]
           {:simple (reduce conj simple (:simple nc*))
            :complex (:complex nc*)}
           ::violated)
         (if (lvar? v)
           meta))
        ::violated))))

(defprotocol IDisequality
  (!=-verify [this sp]))

(extend-type Substitutions
  IDisequality

  (!=-verify [this sp]
             (let [^Substitutions sp sp]
              (cond
               (not sp) this
               (= this sp) nil
               :else (let [[[k v] :as c] (into {} (prefix (.l sp) (.l this)))
                           nc (if (= (count c) 1)
                                {:simple #{v} :complex #{}}
                                {:simple #{} :complex #{c}})
                           ks (keys c)
                           nks (zipmap ks (map #(with-meta %
                                                  (merge-constraints (meta %) nc))
                                               ks))
                           os (.s this)]
                       (make-s (rename-keys os nks)
                               (.l this) constraint))))))

(defn all-different [& lvars]
  (let [c (set lvars)]
    (fn [a]
      (let [clvars (map (fn [lvar]
                          (with-meta lvar
                            {:simple (disj c lvar)
                             :complex #{}}))
                        lvars)]
        (apply unbound* a clvars)))))

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
   {:simple #{:a :b :c} :complex #{{:a 1 :b 2}}}
   {:simple #{:d :e} :complex #{{:d 3 :e 4}}})

  ;; 1.6s
  ;; pretty fast considering
  (dotimes [_ 10]
    (let [c1 {:simple #{:a :b :c} :complex #{{:a 1 :b 2}}}
          c2 {:simple #{:d :e} :complex #{{:d 3 :e 4}}}]
     (time
      (dotimes [_ 1e6]
        (merge-constraints c1 c2)))))

  ;; about the same amount of time
  (dotimes [_ 10]
    (time
     (dotimes [_ 1e6]
       (merge-constraints
        {:simple #{:a :b :c} :complex #{{:a 1 :b 2}}}
        {:simple #{:d :e} :complex #{{:d 3 :e 4}}}))))

  (let [x (lvar 'x)
        y (lvar 'y)
        z (lvar 'z)]
    (-> empty-s
        (unify x [y z])
        (unify y 1)
        .s))
  )