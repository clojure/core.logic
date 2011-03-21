(ns logos.disequality
  (:refer-clojure :exclude [reify == inc])
  (:use [logos.minikanren :exclude [==]]
        logos.match)
  (:import [logos.minikanren Substitutions]))

;; we probably should update reify if we're going to show what constraints there are
;; challenge, how to add disequality constraint without changing things up too much

;; IUnifyWithLVar, all the fns call ext/ext-no-check
;; we could add a verify field to Substitutions, which defaults
;; 
;; (verify u v) => false, ext-no-check returns nil
;; (verify u v) => true, ext-no-check returns the current substitution

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

(extend-type Substitutions
  IDisequality

  (!=-verify [this sp]
              (cond
               (not sp) this
               (= this sp) nil
               :else (let [c (prefix sp this)]
                       ;; add constraint metadata to all the vars in c
                       (Substitutions. {} (.l this)))))
  
  (prefix [this ^Substitutions <s]
          (let [tl (.l this)
                ol (.l <s)]
            (if (= tl ol)
              ()
              (to-s (cons (first tl) (prefix (rest tl) ol)))))))

(defprotocol IUnifyTermVerify
  (unify-term-verify [u v]))

(extend-type logos.minikanren.LVar
  IUnifyTermVerify
  (unify-term-verify [u v a]
    (let [s (unify-term u v a)]
      (if (not= a s)
        (let [m (meta u)]
          (if m
            nil ;; some-stuff
            s))
        s))))

(extend-type Object
  IUnifyTermVerify
  )