(ns logos.disequality
  (:refer-clojure :exclude [reify == inc])
  (:use [logos.minikanren :exclude [==]]
        [clojure.set :only [rename-keys]]
        logos.match)
  (:import [logos.minikanren Substitutions]))

;; all-different ?

(defmacro != [u v]
  `(fn [a#]
     (!=-verify a# (unify a# u v))))

(defmacro == [u v]
  `(fn [a#]
     (!=-verify (unify a# u v) a#)))

(defprotocol IDisequality
  (!=-verify [this sp])
  (==-verify [this u v])
  (prefix [this <s]))

(defn verify-simple [])

(defn verify-complex [])

(defn constraint [s u v]
  (if-let [meta (meta u)]
    (let [{:keys [simple complex]} meta]
      nil)
    true))

(extend-type Substitutions
  IDisequality

  (!=-verify [this sp]
              (cond
               (not sp) this
               (= this sp) nil
               :else (let [[[k v] & r :as c] (into {} (prefix sp this))
                           meta (if (= (count c) 1)
                                  {:simple #{v} :complex []}
                                  {:simple #{} :complex [c]})
                           ks (keys c)
                           nks (zipmap ks (map #(with-meta % meta) ks))
                           os (.s this)]
                       (Substitutions. (rename-keys os nks)
                                       (.l this) constraint))))
  
  (prefix [this <s]
          (let [^Substitutions <s <s
                tl (.l this)
                ol (.l <s)]
            (if (identical? tl ol)
              ()
              (cons (first tl) (prefix (rest tl) ol))))))