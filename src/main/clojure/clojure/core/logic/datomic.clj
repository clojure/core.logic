(defmacro ^:private compile-if
  "Evaluate `exp` and if it returns logical true and doesn't error, expand to
  `then`.  Else expand to `else`.

  (compile-if (Class/forName \"java.util.concurrent.ForkJoinTask\")
    (do-cool-stuff-with-fork-join)
    (fall-back-to-executor-services))"
  [exp then else]
  (if (try (eval exp)
           (catch Throwable _ false))
    `(do ~then)
    `(do ~else)))

(compile-if
 (Class/forName "datomic.Datom")

 (do
   (ns clojure.core.logic.datomic
     (:refer-clojure :exclude [==])
     (:use [clojure.core.logic.protocols]
           [clojure.core.logic]
           [datomic.api :only [db q] :as d]))

   (defn datom? [x]
     (instance? datomic.Datom x))

   (defn unify-with-datom* [u v s]
     (when (and (instance? clojure.lang.PersistentVector v) (> (count v) 1))
       (loop [i 0 v v s s]
         (if (empty? v)
           s
           (when-let [s (unify s (first v) (nth u i))]
             (recur (inc i) (next v) s))))))

   (extend-type datomic.Datom
     IUnifyTerms
     (unify-terms [u v s]
       (unify-with-datom* u v s)))

   (extend-type clojure.lang.PersistentVector
     IUnifyTerms
     (unify-terms [u v s]
       (if (datom? v)
         (unify-with-datom* v u s)
         (when (sequential? v)
           (unify-with-sequential* u v s)))))

   (defn fillq [q]
     (reduce conj q (repeatedly (- 4 (count q)) lvar)))

   (defmulti index-and-components-for
     (fn [a q]
       (->> (fillq q)
            (map (fn [x] (if (lvar? (walk a x)) ::fresh ::ground)))
            (into []))))

   (derive ::fresh ::any)
   (derive ::ground ::any)

   (defmethod index-and-components-for [::ground ::any ::any ::any]
     [a q]
     [:eavt (fillq q)])

   (defmethod index-and-components-for [::fresh ::ground ::fresh ::any]
     [a q]
     (let [[e a v t] (fillq q)]
       [:aevt [a e v t]]))

   (defmethod index-and-components-for [::fresh ::ground ::ground ::any]
     [a q]
     (let [[e a v t] (fillq q)]
       [:avet [a v e t]]))

   (defmethod index-and-components-for [::fresh ::fresh ::ground ::any]
     [a q]
     (let [[e a v t] (fillq q)]
       [:vaet [v a e t]]))

   (defn query [db q]
     (fn [a]
       (let [->id (fn [x]
                    (if (keyword? x)
                      (or (d/entid db x) x)
                      x))
             q (walk a q)
             [index components] (index-and-components-for a q)
             ground-components (->> components
                                    (take-while #(not (lvar? (walk a %))))
                                    (walk* a)
                                    (map ->id))]
         (to-stream
          (map (fn [datom]
                 (unify a (into [] (map ->id q)) datom))
               (apply d/datoms db index ground-components))))))

   (comment
     ;; start transactor from datomic directory
     ;; bin/transactor config/samples/free-transactor-template.properties

     ;; (def uri "datomic:free://localhost:4334/hello")

     ;; ;; only the first time
     ;; (d/create-database uri)

     ;; (def conn (d/connect uri))

     ;; (def schema-tx
     ;;   [{:db/id #db/id[:db.part/db]
     ;;     :db/ident :person/name
     ;;     :db/valueType :db.type/string
     ;;     :db/cardinality :db.cardinality/one
     ;;     :db/doc "A person's name"
     ;;     :db.install/_attribute :db.part/db}])

     ;; (d/transact conn schema-tx)

     ;; (d/transact conn
     ;;             [{:db/id #db/id[:db.part/user]
     ;;               :person/name "Bob"}])

     ;; (q '[:find ?name :where [_ :db.install/attribute ?a] [?a :db/ident ?name]]
     ;;    (db conn))

     ;; (q '[:find ?id :where [?id :db/ident :db.install/attribute]]
     ;;    (db conn))

     ;; (let [db (db conn)]
     ;;   (run* [q]
     ;;     (fresh [e a name]
     ;;       (query db [e :db.install/attribute a])
     ;;       (query db [a :db/ident name])
     ;;       (== q name))))
     )
   )

 (do
   (ns clojure.core.logic.datomic))
 )

