core.logic
====

A Logic Programming library for Clojure. At its heart is an original implementation of miniKanren as described in William Byrd's dissertation [Relational Programming in miniKanren: Techniques, Applications, and Implementations](http://pqdtopen.proquest.com/#abstract?dispub=3380156). It's also described in great detail in the [The Reasoned Schemer](http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=10663). However, do note that the version that appears in The Reasoned Schemer is an earlier implementation and differs from the one on which this library is based.

Performance is a central concern of this project. Anything that makes it slower will probably not be adopted. Anything that makes it faster without overly complicating the implementation will be considered. It would be interesting to see how we fare on the standard Prolog benchmarks. Currently, on my machine, solving the classic Zebra puzzle 1000 times takes SWI-Prolog about 6 seconds, it takes core.logic running under Clojure 1.3.0-alpha7 less then 2s without <code>occurs-check</code>.

If you wish to work through The Reasoned Schemer with core.logic make sure to look over [this](https://github.com/clojure/core.logic/wiki/Differences-from-The-Reasoned-Schemer) first.

Roadmap
----

The following are tentative current and future directions:

* **Environment Trimming** - Definite Clause Grammars (DCGs) are quite slow in miniKanren. This may be due to a lack of groundness analysis or it may be because we are not trimming the environment of needless logic variables.
* **Constraint Logic Programming** - Constraint Handling Rules (CHR) is particularly inspiring. William Byrd and Daniel Friedman are working on CLP(FD) and CLP(X) extensions to miniKanren. We should incorporate this.
* **Groundness Analysis** - Initial research on feasibility done. It does in fact give significant performance boosts (2-3X). Seems to close many performance gaps between SWI-Prolog and miniKanren. However maintaining correctness seems difficult. Perhaps limit optimization to DCGs and pattern matching sugar.
* **Negation** - Stratified Negation as provided by XSB ?

Examples
----

A classic AI program:

```clj
(use '[clojure.core.logic minikanren prelude])

(defne moveo [before action after]
  ([[:middle :onbox :middle :hasnot]
    :grasp
    [:middle :onbox :middle :has]])
  ([[?pos :onfloor ?pos ?has]
    :climb
    [?pos :onbox ?pos ?has]])
  ([[?pos1 :onfloor ?pos1 ?has]
    :push
    [?pos2 :onfloor ?pos2 ?has]])
  ([[?pos1 :onfloor ?box ?has]
    :walk
    [?pos2 :onfloor ?box ?has]]))

(defne cangeto [state out]
  ([[_ _ _ :has] true])
  ([_ _] (fresh [action next]
           (moveo state action next)
           (cangeto next out))))

(run 1 [q]
  (cangeto [:atdoor :onfloor :atwindow :hasnot] q)) ; (true)
```

A classic Prolog program:

```prolog
append([] ,Y,Y).
append([A|D],Y2,[A|R]) :- append(D,Y2,R).
```

The core.logic version is almost equally succinct:

```clj
(defne appendo [x y z]
  ([() _ y])
  ([[?a . ?d] _ [?a . ?r]] (appendo ?d y ?r)))
```

Here's a simple type inferencer for the simply typed lambda calculus based on a version originally written in Prolog:

```clj
(use '[clojure.core.logic minikanren prelude nonrel match])

(defna findo [x l o]
  ([_ [[?y :- o] . _] _] 
    (project [x ?y] (== (= x ?y) true)))
  ([_ [_ . ?c] _] (findo x ?c o)))

(defn typedo [c x t]
  (conda
    ((lvaro x) (findo x c t))
    ((matche [c x t]
       ([_ [[?x] :>> ?a] [?s :> ?t]]
          (fresh [l]
            (conso [?x :- ?s] c l)
            (typedo l ?a ?t)))
       ([_ [:apply ?a ?b] _]
          (fresh [s]
            (typedo c ?a [s :> t])
            (typedo c ?b s)))))))

(comment
  ;; ([_.0 :> _.1])
  (run* [q]
    (fresh [f g a b t]
     (typedo [[f :- a] [g :- b]] [:apply f g] t)
     (== q a)))

  ;; ([:int :> _.0])
  (run* [q]
    (fresh [f g a t]
     (typedo [[f :- a] [g :- :int]] [:apply f g] t)
     (== q a)))

  ;; (:int)
  (run* [q]
    (fresh [f g a t]
     (typedo [[f :- [:int :> :float]] [g :- a]] 
       [:apply f g] t)
     (== q a)))

  ;; ()
  (run* [t]
    (fresh [f a b]
      (typedo [f :- a] [:apply f f] t)))

  ;; ([_.0 :> [[_.0 :> _.1] :> _.1]])
  (run* [t]
    (fresh [x y]
      (typedo [] 
        [[x] :>> [[y] :>> [:apply y x]]] t)))
  )
```

Tabling
----

core.logic as of version 0.5.4 supports tabling. Certain kinds of logic programs that would not terminate in Prolog will terminate in core.logic if you create a tabled goal.

```clj
(defne arco [x y]
  ([:a :b])
  ([:b :a])
  ([:b :d]))

(def patho
  (tabled [x y]
    (conde
     ((arco x y))
     ((fresh [z]
        (arco x z)
        (patho z y))))))

;; (:b :a :d)
(run* [q] (patho :a q))
```

Disequality
----

core.logic supports disequality constraints.

```clj
(run* [q]
  (fresh [x y]
    (!= [x 2] [y 1])
    (== x 1)
    (== y 3)
    (== q [x y]))) ; ([1 3])

(run* [q]
  (fresh [x y]
    (!= [x 2] [y 1])
    (== x 1)
    (== y 2)
    (== q [x y]))) ; ()
```

Unification
----

core.logic comes with a unifier that can be used much like [core.unify](https://github.com/clojure/core.unify):

```clj
(unifier '(?x ?y ?z) '(1 2 ?y)) ; (1 2 _.0)
```

The above is quite slow since we have to walk the data structure and replace the logic var symbols. It's more efficient to <code>prep</code> the expressions before hand if you're going to be unifying the same expressions over and over again.

```clj
(let [[u w] (map prep ['(?x ?y) (1 2)])]
  (unifier* u w))
```

Definite Clause Grammars
----

core.logic has Prolog-type DCG syntax for parsing:

```clj
(def-->e verb [v]
  ([[:v 'eats]] '[eats]))

(def-->e noun [n]
  ([[:n 'bat]] '[bat])
  ([[:n 'cat]] '[cat]))

(def-->e det [d]
  ([[:d 'the]] '[the])
  ([[:d 'a]] '[a]))

(def-->e noun-phrase [n]
  ([[:np ?d ?n]] (det ?d) (noun ?n)))

(def-->e verb-phrase [n]
  ([[:vp ?v ?np]] (verb ?v) (noun-phrase ?np)))

(def-->e sentence [s]
  ([[:s ?np ?vp]] (noun-phrase ?np) (verb-phrase ?vp)))

(run* [parse-tree]
  (sentence parse-tree '[the bat eats a cat] []))

;; ([:s [:np [:d the] [:n bat]] [:vp [:v eats] [:np [:d a] [:n cat]]]])
```

Defining facts
----

Sometimes it's useful to create a list of facts that you want to run queries over. Use <code>defrel</code> and <code>fact</code>.

```clj
(defrel man p)
(fact man 'Bob)
(fact man 'John)
(fact man 'Ricky)

(defrel woman p)
(fact woman 'Mary)
(fact woman 'Martha)
(fact woman 'Lucy)
    
(defrel likes p1 p2)
(fact likes 'Bob 'Mary)
(fact likes 'John 'Martha)
(fact likes 'Ricky 'Lucy)

(defrel fun p)
(fact fun 'Lucy)

(run* [q]
  (fresh [x y]
    (fun y)
    (likes x y)
    (== q [x y]))) ; ([Ricky Lucy])
```

It's important to index relationships so that the time to run queries doesn't grow linearly with the number of facts. You can create indexes for any element of the fact tuple. Note that this has implications for memory usage.

```clj
; Clojure 1.3.0
(defrel likes ^:index p1 ^:index p2)

; Clojure 1.2.0
(defrel likes ^{:index true} p1 ^{:index true} p2)
```

YourKit
----

YourKit has has given me a free license for their profiler, greatly simplifying the profiling of core.logic performance. 

YourKit is kindly supporting open source projects with its full-featured Java Profiler. YourKit, LLC is the creator of innovative and intelligent tools for profiling Java and .NET applications. Take a look at YourKit's leading software products:

* <a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
* <a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.

Notes
----

I stayed pretty true to the ideas of the original implementation. There are however several key differences. Unification uses protocols in order leverage the full speed of the host. Clojure's cons operator differs significantly from Scheme's so I added the <code>LConsSeq</code> protocol. Sequences which end in a logic variables can be represented by using <code>lcons</code>

```clj
(lcons 'a (lvar 'b)) ; (a . <lvar:b>)
```

Logic variables are instances of <code>LVar</code> not vectors as they are in Scheme.

The goal and goal constructor portion has been written from scratch on top of the protocols and makes liberal use of lazy sequences to prevent stack overflow.

Currently the <code>Substitutions</code> deftype uses <code>clojure.lang.PersistentHashMap</code> internally. This may be replaced with something that provides better performance for triangular substitutions.

Goals
----

* Simplicity. Optimizations should not destroy the *ideas* behind the original design. Any person willing to take take the time to understand the original Scheme implementation should have little trouble understanding how core.logic is put together.
* Performance. This implementation is faster than miniKanren under Racket and seems to be close to performnace of miniKanren recorded by the original designers when running under Chez Scheme.
* Emphasis on pure relational programming.

Resources
----

* [Efficient Constraint Propagation Engines](http://www.gecode.org/paper.html?id=SchulteStuckey:TOPLAS:2008)
* [Techniques for Efficient Constraint Propagation](http://www.gecode.org/paper.html?id=Lagerkvist:Lic:Diss:2008)
* [Operations Research Tools developed at Google](http://code.google.com/p/or-tools/_)
* [logilab-constraint](http://hg.logilab.org/logilab/constraint)
* [Solving Every Sudoku Puzzle](http://norvig.com/sudoku.html)
* [Constraint Handling Rules](http://www.informatik.uni-ulm.de/pm/fileadmin/pm/home/fruehwirth/constraint-handling-rules-book.html)
* [The XSB System Version 3.2 - Volume 2: Libraries, Interfaces, and Packages](http://xsb.sourceforge.net/manual2/manual2.pdf), particularly the section on Attributed Variables
* [The XSB System Version 3.2 - Volume 1: Programmer's Manual](http://xsb.sourceforge.net/manual1/manual1.pdf)
* [Concepts, Technqiues, and Models of Computer Programming](http://www.info.ucl.ac.be/~pvr/book.html), Chapters 9 and 12
* [Art of the Propagator](http://dspace.mit.edu/handle/1721.1/44215)
* [Constraint Propagation - Models, Techniques, Implementation](http://people.cs.kuleuven.be/~guido.tack/dissertation.php)
* [Relational Programming in miniKanren: Techniques, Applications, and Implementations](http://pqdtopen.proquest.com/#abstract?dispub=3380156)
* [The Reasoned Schemer](http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=10663)
* [Efficient representations for triangular substitutions: A comparison in miniKanren](https://www.cs.indiana.edu/~lkuper/papers/walk.pdf)
* [A pattern matcher for miniKanren, or, how to get into trouble with CPS macros](http://www.cs.indiana.edu/~lkuper/papers/lambdae.pdf)
* [Kanren](http://kanren.sourceforge.net/)
* [Logical JVM: Implementing the miniKanren logic system in Scala](http://hircus.multics.org/kanren/presentation.html)
* [minikanren-scala](https://github.com/hircus/minikanren-scala)
* [Purely Functional Data Strucutres](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.64.3080&rep=rep1&type=pdf)
* [Using Datalog with Binary Decision Diagrams for Program Analysis](http://people.csail.mit.edu/mcarbin/papers/aplas05.pdf)
* [Memoing for Logic Programs](http://portal.acm.org/citation.cfm?id=131299)
* [Efficient bottom-up abstract interpretation of prolog by means of constraint solving over symbolic finite domains](http://portal.acm.org/citation.cfm?id=692605)

Distributed under the Eclipse Public License, the same as Clojure.