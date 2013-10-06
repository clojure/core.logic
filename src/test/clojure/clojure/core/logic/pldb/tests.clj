(ns clojure.core.logic.pldb.tests
  (:use [clojure.test])
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

;; from core.logic tests
(pldb/db-rel man p)
(pldb/db-rel woman p)
(pldb/db-rel likes p1 p2)
(pldb/db-rel fun p)

(def facts0
  (pldb/db
   [man 'Bob]
   [man 'John]
   [man 'Ricky]

   [woman 'Mary]
   [woman 'Martha]
   [woman 'Lucy]

   [likes 'Bob 'Mary]
   [likes 'John 'Martha]
   [likes 'Ricky 'Lucy]))

(def facts1 (-> facts0
                (pldb/db-fact fun 'Lucy)))

(deftest test-facts0
  ( pldb/with-db facts0
    (is (=
         (l/run* [q]
                 (l/fresh [x y]
                          (likes x y)
                          (fun y)
                          (l/== q [x y])))
         '()))))

(deftest test-facts1
  (pldb/with-db facts1
    (is (=
         (l/run* [q]
                 (l/fresh [x y]
                          (likes x y)
                          (fun y)
                          (l/== q [x y])))
         '([Ricky Lucy])))))

(def facts1-retracted
  (-> facts1
      (pldb/db-retraction likes 'Bob 'Mary)))

(deftest test-rel-retract
  (pldb/with-db facts1-retracted
    (is (= (into #{}
                 (l/run* [q]
                         (l/fresh [x y]
                                  (likes x y)
                                  (l/== q [x y]))))
           (into #{} '([John Martha] [Ricky Lucy]))))))

(pldb/db-rel rel1 ^:index a)
(def indexed-db
  (pldb/db [rel1 [1 2]]))

(deftest test-rel-logic-29
  (pldb/with-db indexed-db
    (is (=
         (l/run* [q]
                 (l/fresh [a]
                          (rel1 [q a])
                          (l/== a 2)))
         '(1)))))

(pldb/db-rel rel2 ^:index e ^:index a ^:index v)
(def facts2
  (pldb/db
   [rel2 :e1 :a1 :v1]
   [rel2 :e1 :a2 :v2]))

(def facts2-retracted1
  (pldb/db-retractions facts2
                       [rel2 :e1 :a1 :v1]))

(def facts2-retracted2
  (pldb/db-retractions facts2
                       [rel2 :e1 :a2 :v2]))

(def facts2-retracted-all
  (pldb/db-retractions facts2
                       [rel2 :e1 :a1 :v1]
                       [rel2 :e1 :a2 :v2]))

(deftest rel2-dup-retractions
  (is (= #{[:e1 :a1 :v1] [:e1 :a2 :v2]}
         (pldb/with-db facts2
           (into #{}
                 (l/run* [out]
                         (l/fresh [e a v]
                                  (rel2 e :a1 :v1)
                                  (rel2 e a v)
                                  (l/== [e a v] out)))))))
  (is (= #{}
         (pldb/with-db facts2-retracted1
           (into #{}
                 (l/run* [out]
                         (l/fresh [e a v]
                                  (rel2 e :a1 :v1)
                                  (rel2 e a v)
                                  (l/== [e a v] out)))))))
  (is (= #{[:e1 :a1 :v1]}
         (pldb/with-db facts2-retracted2
           (into #{}
                 (l/run* [out]
                         (l/fresh [e a v]
                                  (rel2 e :a1 :v1)
                                  (rel2 e a v)
                                  (l/== [e a v] out)))))))
  (is (= #{}
         (pldb/with-db facts2-retracted-all
           (into #{}
                 (l/run* [out]
                         (l/fresh [e a v]
                                  (rel2 e :a1 :v1)
                                  (rel2 e a v)
                                  (l/== [e a v] out))))))))


;; ----------------------------------------

(pldb/db-rel protocol name port-number)
(pldb/db-rel open-port ip port-number)

(def known-ports
  (pldb/db
   [protocol :ftp 21]
   [protocol :ssh 22]
   [protocol :telnet 23]
   [protocol :smtp 25]
   [protocol :http 80]
   [protocol :pop3 110]
   [protocol :imap 143]
   [protocol :ldap 389]
   [protocol :https 443]))

(def network1
  (pldb/db
   [open-port :10.0.1.3 22]
   [open-port :10.0.1.5 22]
   [open-port :10.0.1.8 22]
   [open-port :10.0.1.8 80]
   [open-port :10.0.1.12 22]
   [open-port :10.0.1.19 22]
   [open-port :10.0.1.19 25]
   [open-port :10.0.1.19 143]
   [open-port :10.0.1.136 22]
   [open-port :10.0.1.136 80]
   [open-port :10.0.1.136 443]))

(def network2
  (pldb/db
   [open-port :192.168.128.213 22]
   [open-port :192.168.128.213 443]
   [open-port :192.168.128.217 22]
   [open-port :192.168.128.217 80]
   [open-port :192.168.128.217 443]
   [open-port :192.168.128.199 22]
   [open-port :192.168.128.140 22]
   [open-port :192.168.128.140 25]
   [open-port :192.168.128.140 110]
   [open-port :192.168.128.140 143]
   [open-port :192.168.128.140 389]))


(deftest merge-same-relationship
  (is (= #{:10.0.1.19}
         (pldb/with-db network1
           (set (l/run* [ip]
                        (open-port ip 143))))))

  (is (= #{:192.168.128.140}
         (pldb/with-db network2
           (set (l/run* [ip]
                        (open-port ip 143))))))

  (is (= #{:192.168.128.140 :10.0.1.19}
         (pldb/with-db network1
           (pldb/with-db network2
             (set (l/run* [ip]
                          (open-port ip 143)))))))

  (is (= #{:192.168.128.140 :10.0.1.19}
         (pldb/with-db network2
           (pldb/with-db network1
             (set (l/run* [ip]
                          (open-port ip 143)))))))

  (is (= #{:192.168.128.140 :10.0.1.19}
         (pldb/with-dbs [network1 network2]
           (set (l/run* [ip]
                        (open-port ip 143)))))))

(deftest merge-across-relationship
  (is (= #{:10.0.1.136 :192.168.128.217}
         (pldb/with-dbs [known-ports network1 network2]
           (set (l/run* [ip]
                        (l/fresh [http-port https-port]
                                 (protocol :http http-port)
                                 (protocol :https https-port)
                                 (open-port ip http-port)
                                 (open-port ip https-port))))))))



;; ----------------------------------------

(pldb/db-rel rps move)
(def moves-db (pldb/db
  [rps :rock]
  [rps :paper]
  [rps :scissors]))

(deftest test-lazy
  (is (= (into #{}
               (pldb/with-db moves-db
                 (l/run* [q] (rps q))))

         (pldb/with-db moves-db
           (into #{}
                 (l/run* [q] (rps q))))

         #{:rock :paper :scissors})))
