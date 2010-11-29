(ns logos.repl
  (:use [logos.minikanren :only [run run* == exist s# u# cond-e]]))

(defn not-quit? [s]
  (let [e (read-string s)]
   (if (= e '(quit))
     ::exit
     (eval e))))

(defn repl []
  (.print System/out "?- ")
  (.flush System/out)
  (let [v (not-quit? (read-line))]
    (when (not= v ::exit) 
      (println v)
      (recur))))
