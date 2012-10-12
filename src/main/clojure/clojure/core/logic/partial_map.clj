;;This file defines a PMap (partial map) record that unifies with maps without having all of that map's keys.
(in-ns 'clojure.core.logic)

(defprotocol IUnifyWithPMap
  (unify-with-pmap [pmap u s]))

(defrecord PMap []
  IUnifyWithMap
  (unify-with-map [v u s]
    (loop [ks (keys v) s s]
      (if (seq ks)
        (let [kf (first ks)
              uf (get u kf ::not-found)]
          (if (= uf ::not-found)
            nil
            (if-let [s (unify s (get v kf) uf)]
              (recur (next ks) s)
              nil)))
        s)))

  IUnifyWithPMap
  (unify-with-pmap [v u s]
    (unify-with-map v u s))

  IUnifyTerms
  (unify-terms [u v s]
    (unify-with-pmap v u s))

  IUnifyWithLVar
  (unify-with-lvar [v u s]
    (ext-no-check s u v)))

(extend-protocol IUnifyWithPMap
  nil
  (unify-with-pmap [v u s] nil)

  Object
  (unify-with-pmap [v u s] nil)

  clojure.core.logic.LVar
  (unify-with-pmap [v u s]
    (ext s v u))

  clojure.lang.IPersistentMap
  (unify-with-pmap [v u s]
    (unify-with-map u v s)))

(defn partial-map
  "Given map m, returns partial map that unifies with maps even if it doesn't share all of the keys of that map.
   Only the keys of the partial map will be unified:

   (run* [q]
         (fresh [pm x]
                (== pm (partial-map {:a x}))
                (== pm {:a 1 :b 2})
                (== pm q)))
   ;;=> ({:a 1})"
  [m]
  (map->PMap m))
