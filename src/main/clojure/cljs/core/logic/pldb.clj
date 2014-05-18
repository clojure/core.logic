(ns cljs.core.logic.pldb)

(defn indexed? [v]
  (true? (:index (meta v))))

(defmacro with-dbs [dbs & body]
  `(binding [cljs.core.logic/*logic-dbs* (concat cljs.core.logic/*logic-dbs* ~dbs)]
          ~@body))

(defmacro with-db [db & body]
  `(binding [cljs.core.logic/*logic-dbs* (conj cljs.core.logic/*logic-dbs* ~db)]
          ~@body))

(defmacro db-rel [name & args]
  (let [arity
        (count args)

        kname
        (str name "_" arity)

        indexes
        (vec (map indexed? args))]
    `(def ~name
       (with-meta
         (fn [& query#]
           (fn [subs#]
             (let [dbs#
                   (-> subs# cljs.core/meta :db)

                   facts#
                   (if-let [index# (index-for-query subs# query# ~indexes)]
                     (facts-using-index dbs#
                                        ~kname
                                        index#
                                        (cljs.core.logic/walk* subs# (nth query# index#)))
                     (facts-for dbs# ~kname))]
               (cljs.core.logic/to-stream (map (fn [potential#]
                                   ((cljs.core.logic/== query# potential#) subs#))
                                 facts#)))))
         {:rel-name ~kname
          :indexes ~indexes}))))
