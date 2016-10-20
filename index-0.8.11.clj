{:namespaces
 ({:doc nil,
   :name "clojure.core.logic",
   :wiki-url "http://clojure.github.io/core.logic/index.html",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj"}
  {:doc nil,
   :name "clojure.core.logic.arithmetic",
   :wiki-url
   "http://clojure.github.io/core.logic/index.html#clojure.core.logic.arithmetic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj"}
  {:doc nil,
   :name "clojure.core.logic.bench",
   :wiki-url
   "http://clojure.github.io/core.logic/index.html#clojure.core.logic.bench",
   :source-url
   "https://github.com/clojure/core.logic/blob/b256c9ffe3b7fa46850cb3960414571c62293cb7/src/main/clojure/clojure/core/logic/bench.clj"}
  {:doc nil,
   :name "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic/index.html#clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj"}
  {:doc nil,
   :name "clojure.core.logic.nominal",
   :wiki-url
   "http://clojure.github.io/core.logic/index.html#clojure.core.logic.nominal",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj"}
  {:doc nil,
   :name "clojure.core.logic.unifier",
   :wiki-url
   "http://clojure.github.io/core.logic/index.html#clojure.core.logic.unifier",
   :source-url
   "https://github.com/clojure/core.logic/blob/dba7c697ca45a7fee7595cd18d057c420261d498/src/main/clojure/clojure/core/logic/unifier.clj"}),
 :vars
 ({:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "!=",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L2458",
   :line 2458,
   :var-type "function",
   :arglists ([u v]),
   :doc
   "Disequality constraint. Ensures that u and v will never\nunify. u and v can be complex terms.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/!="}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "->AnswerCache",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1818",
   :line 1818,
   :var-type "function",
   :arglists ([ansl anss _meta]),
   :doc
   "Positional factory function for class clojure.core.logic.AnswerCache.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/->AnswerCache"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "->Choice",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1084",
   :line 1084,
   :var-type "function",
   :arglists ([a f]),
   :doc
   "Positional factory function for class clojure.core.logic.Choice.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/->Choice"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "->ConstraintStore",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L109",
   :line 109,
   :var-type "function",
   :arglists ([km cm cid running]),
   :doc
   "Positional factory function for class clojure.core.logic.ConstraintStore.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/->ConstraintStore"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "->LCons",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L755",
   :line 755,
   :var-type "function",
   :arglists ([a d cache meta]),
   :doc
   "Positional factory function for class clojure.core.logic.LCons.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/->LCons"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "->LVar",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L621",
   :line 621,
   :var-type "function",
   :arglists ([id unique name oname hash meta]),
   :doc
   "Positional factory function for class clojure.core.logic.LVar.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/->LVar"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "->PMap",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L2518",
   :line 2518,
   :var-type "function",
   :arglists ([]),
   :doc
   "Positional factory function for class clojure.core.logic.PMap.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/->PMap"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "->Pair",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L43",
   :line 43,
   :var-type "function",
   :arglists ([lhs rhs]),
   :doc
   "Positional factory function for class clojure.core.logic.Pair.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/->Pair"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "->SubstValue",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L197",
   :line 197,
   :var-type "function",
   :arglists ([v doms eset]),
   :doc
   "Positional factory function for class clojure.core.logic.SubstValue.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/->SubstValue"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "->Substitutions",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L287",
   :line 287,
   :var-type "function",
   :arglists ([s vs ts cs cq cqs oc _meta]),
   :doc
   "Positional factory function for class clojure.core.logic.Substitutions.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/->Substitutions"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "->SuspendedStream",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1849",
   :line 1849,
   :var-type "function",
   :arglists ([cache ansv* f]),
   :doc
   "Positional factory function for class clojure.core.logic.SuspendedStream.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/->SuspendedStream"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "==",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1156",
   :line 1156,
   :var-type "function",
   :arglists ([u v]),
   :doc "A goal that attempts to unify terms u and v.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/=="}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "all",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1268",
   :line 1268,
   :var-type "macro",
   :arglists ([] [& goals]),
   :doc "Like fresh but does does not create logic variables.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/all"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "and*",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1273",
   :line 1273,
   :var-type "function",
   :arglists ([goals]),
   :doc
   "A function version of all, which takes a list of goals and succeeds only fi they all succeed.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/and*"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "appendo",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1772",
   :line 1772,
   :var-type "function",
   :arglists ([x y z]),
   :doc
   "A relation where x, y, and z are proper collections,\nsuch that z is x appended to y",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/appendo"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "conda",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1417",
   :line 1417,
   :var-type "macro",
   :arglists ([& clauses]),
   :doc
   "Soft cut. Once the head of a clause has succeeded\nall other clauses will be ignored. Non-relational.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/conda"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "conde",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1175",
   :line 1175,
   :var-type "macro",
   :arglists ([& clauses]),
   :doc
   "Logical disjunction of the clauses. The first goal in\na clause is considered the head of that clause. Interleaves the\nexecution of the clauses.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/conde"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "condu",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1425",
   :line 1425,
   :var-type "macro",
   :arglists ([& clauses]),
   :doc
   "Committed choice. Once the head (first goal) of a clause\nhas succeeded, remaining goals of the clause will only\nbe run once. Non-relational.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/condu"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "conjo",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L2802",
   :line 2802,
   :var-type "function",
   :arglists ([coll & args]),
   :doc "A constraint version of conj",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/conjo"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "conso",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1659",
   :line 1659,
   :var-type "function",
   :arglists ([a d l]),
   :doc
   "A relation where l is a collection, such that a is the first of l\nand d is the rest of l. If ground d must be bound to a proper tail.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/conso"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "copy-term",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1439",
   :line 1439,
   :var-type "function",
   :arglists ([u v]),
   :doc "Copies a term u into v. Non-relational.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/copy-term"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "defna",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1728",
   :line 1728,
   :var-type "macro",
   :arglists ([& rest]),
   :doc "Define a soft cut goal. See conda.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/defna"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "defne",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1699",
   :line 1699,
   :var-type "macro",
   :arglists ([& rest]),
   :doc
   "Define a goal fn. Supports pattern matching. All\npatterns will be tried. See conde.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/defne"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "defnu",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1733",
   :line 1733,
   :var-type "macro",
   :arglists ([& rest]),
   :doc "Define a committed choice goal. See condu.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/defnu"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "distincto",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L2472",
   :line 2472,
   :var-type "function",
   :arglists ([l]),
   :doc
   "A relation which guarantees no element of l will unify\nwith another element of l.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/distincto"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "emptyo",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1654",
   :line 1654,
   :var-type "function",
   :arglists ([a]),
   :doc "A relation where a is the empty list",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/emptyo"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "everyg",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1677",
   :line 1677,
   :var-type "function",
   :arglists ([g coll]),
   :doc
   "A pseudo-relation that takes a coll and ensures that the goal g\nsucceeds on every element of the collection.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/everyg"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "fail",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1144",
   :line 1144,
   :var-type "function",
   :arglists ([a]),
   :doc "A goal that always fails.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/fail"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "featurec",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L2579",
   :line 2579,
   :var-type "function",
   :arglists ([x fs]),
   :doc
   "Ensure that a map contains at least the key-value pairs\nin the map fs. fs must be partially instantiated - that is,\nit may contain values which are logic variables to support\nfeature extraction.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/featurec"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "firsto",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1665",
   :line 1665,
   :var-type "function",
   :arglists ([l a]),
   :doc
   "A relation where l is a collection, such that a is the first of l",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/firsto"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "fix-constraints",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L2088",
   :line 2088,
   :var-type "function",
   :arglists ([a]),
   :doc
   "A goal to run the constraints in cq until it is empty. Of\ncourse running a constraint may grow cq so this function\nfinds the fixpoint.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/fix-constraints"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "fna",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1718",
   :line 1718,
   :var-type "macro",
   :arglists ([& rest]),
   :doc "Define an anonymous soft cut goal. See conda.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/fna"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "fnc",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L2611",
   :line 2611,
   :var-type "macro",
   :arglists ([args & body]),
   :doc
   "Define an anonymous constraint that can be used with the unifier:\n\n   (let [oddc (fnc [x] (odd? x))]\n\n     (unifier {:a '?a} {:a 1} :when {'?a oddc})\n       ;;=> {:a 1}\n\n     (unifier {:a '?a} {:a 2} :when {'?a oddc})\n       ;;=> nil\n   )\n\nNote, the constraint will not run until all arguments are fully ground.\n\nUse defnc to define a constraint and assign a toplevel var.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/fnc"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "fne",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1693",
   :line 1693,
   :var-type "macro",
   :arglists ([& rest]),
   :doc
   "Define an anonymous goal fn. Supports pattern matching. All\npatterns will be tried. See conde.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/fne"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "fnu",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1723",
   :line 1723,
   :var-type "macro",
   :arglists ([& rest]),
   :doc "Define an anonymous committed choice goal. See condu.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/fnu"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "fresh",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1207",
   :line 1207,
   :var-type "macro",
   :arglists ([[& lvars] & goals]),
   :doc
   "Creates fresh variables. Goals occuring within form a logical\nconjunction.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/fresh"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "is",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1344",
   :line 1344,
   :var-type "macro",
   :arglists ([u v op]),
   :doc
   "Set the value of a var to value of another var with the operation\napplied. Non-relational.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/is"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "lcons",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L868",
   :line 868,
   :var-type "function",
   :arglists ([a d]),
   :doc
   "Constructs a sequence a with an improper tail d if d is a logic variable.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/lcons"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "llist",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L878",
   :line 878,
   :var-type "macro",
   :arglists ([f s] [f s & rest]),
   :doc
   "Constructs a sequence from 2 or more arguments, with the last argument as the\ntail. The tail is improper if the last argument is a logic variable.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/llist"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "log",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1288",
   :line 1288,
   :var-type "macro",
   :arglists ([& s]),
   :doc "Goal for println",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/log"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "lvaro",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1448",
   :line 1448,
   :var-type "macro",
   :arglists ([v]),
   :doc
   "A goal that succeeds if the argument is fresh. v must be a logic\nvariable. Non-relational.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/lvaro"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "map->PMap",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L2518",
   :line 2518,
   :var-type "function",
   :arglists ([m#]),
   :doc
   "Factory function for class clojure.core.logic.PMap, taking a map of keywords to field values.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/map->PMap"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "map->SubstValue",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L197",
   :line 197,
   :var-type "function",
   :arglists ([m#]),
   :doc
   "Factory function for class clojure.core.logic.SubstValue, taking a map of keywords to field values.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/map->SubstValue"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "map->SuspendedStream",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1849",
   :line 1849,
   :var-type "function",
   :arglists ([m#]),
   :doc
   "Factory function for class clojure.core.logic.SuspendedStream, taking a map of keywords to field values.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/map->SuspendedStream"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "master",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1976",
   :line 1976,
   :var-type "function",
   :arglists ([argv cache]),
   :doc
   "Take the argument to the goal and check that we don't\nhave an alpha equivalent cached answer term in the cache.\nIf it doesn't already exist in the cache add the new\nanswer term.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/master"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "matcha",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1738",
   :line 1738,
   :var-type "macro",
   :arglists ([xs & cs]),
   :doc "Define a soft cut pattern match. See conda.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/matcha"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "matche",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1705",
   :line 1705,
   :var-type "macro",
   :arglists ([xs & cs]),
   :doc
   "Pattern matching macro. All patterns will be tried.\nSee conde.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/matche"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "matchu",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1744",
   :line 1744,
   :var-type "macro",
   :arglists ([xs & cs]),
   :doc "Define a committed choice goal. See condu.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/matchu"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "member1o",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1762",
   :line 1762,
   :var-type "function",
   :arglists ([x l]),
   :doc
   "Like membero but uses to disequality further constraining\nthe results. For example, if x and l are ground and x occurs\nmultiple times in l, member1o will succeed only once.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/member1o"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "membero",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1755",
   :line 1755,
   :var-type "function",
   :arglists ([x l]),
   :doc "A relation where l is a collection, such that l contains x.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/membero"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "nafc",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L2725",
   :line 2725,
   :var-type "function",
   :arglists ([c & args]),
   :doc
   "EXPERIMENTAL: negation as failure constraint. All arguments to the goal c\nmust be ground. If some argument is not ground the execution of this constraint\nwill be delayed.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/nafc"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "nilo",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1649",
   :line 1649,
   :var-type "function",
   :arglists ([a]),
   :doc "A relation where a is nil",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/nilo"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "nonlvaro",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1456",
   :line 1456,
   :var-type "macro",
   :arglists ([v]),
   :doc
   "A goal that succeeds if the argument is not fresh. v must be a\nlogic variable. Non-relational.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/nonlvaro"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "or*",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1185",
   :line 1185,
   :var-type "function",
   :arglists ([goals]),
   :doc
   "A function version of conde, which takes a list of goals and tries them as if via conde.\nNote that or* only does disjunction, ie (or* [a b c]) is the same as (conde [a] [b] [c]).\nIf you need something like (conde [a b] [c]), you can use and*, or all:\n(or* [(and* a b) c]).",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/or*"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "partial-map",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L2536",
   :line 2536,
   :var-type "function",
   :arglists ([m]),
   :doc
   "Given map m, returns partial map that unifies with maps even if it\ndoesn't share all of the keys of that map.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/partial-map"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "permuteo",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1781",
   :line 1781,
   :var-type "function",
   :arglists ([xl yl]),
   :doc
   "A relation that will permute xl into the yl. May not\nterminate if xl is not ground.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/permuteo"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "pred",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1336",
   :line 1336,
   :var-type "macro",
   :arglists ([v f]),
   :doc
   "Check a predicate against the value logic var. Non-relational.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/pred"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "project",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1327",
   :line 1327,
   :var-type "macro",
   :arglists ([[& vars] & goals]),
   :doc
   "Extract the values bound to the specified logic vars. Non-relational.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/project"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "rembero",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L2483",
   :line 2483,
   :var-type "function",
   :arglists ([x l o]),
   :doc
   "A relation between l and o where x is removed from\nl exactly one time.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/rembero"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "resto",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1671",
   :line 1671,
   :var-type "function",
   :arglists ([l d]),
   :doc
   "A relation where l is a collection, such that d is the rest of l",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/resto"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "run",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1237",
   :line 1237,
   :var-type "macro",
   :arglists ([n bindings & goals]),
   :doc "Executes goals until a maximum of n results are found.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/run"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "run*",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1242",
   :line 1242,
   :var-type "macro",
   :arglists ([bindings & goals]),
   :doc "Executes goals until results are exhausted.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/run*"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "run-db",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1247",
   :line 1247,
   :var-type "macro",
   :arglists ([n db bindings & goals]),
   :doc
   "Executes goals until a maximum of n results are found. Uses a specified logic database.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/run-db"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "run-db*",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1252",
   :line 1252,
   :var-type "macro",
   :arglists ([db bindings & goals]),
   :doc
   "Executes goals until results are exhausted. Uses a specified logic database.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/run-db*"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "run-nc",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1257",
   :line 1257,
   :var-type "macro",
   :arglists ([n bindings & goals]),
   :doc
   "Executes goals until a maximum of n results are found. Does not\noccurs-check.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/run-nc"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "run-nc*",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1263",
   :line 1263,
   :var-type "macro",
   :arglists ([& goals]),
   :doc
   "Executes goals until results are exhausted. Does not occurs-check.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/run-nc*"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "succeed",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1140",
   :line 1140,
   :var-type "function",
   :arglists ([a]),
   :doc "A goal that always succeeds.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/succeed"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "tabled",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1997",
   :line 1997,
   :var-type "macro",
   :arglists ([args & grest]),
   :doc
   "Macro for defining a tabled goal. Prefer ^:tabled with the\ndefne/a/u forms over using this directly.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/tabled"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "trace-lvars",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1305",
   :line 1305,
   :var-type "macro",
   :arglists ([title & lvars]),
   :doc "Goal for tracing the values of logic variables.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/trace-lvars"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "trace-s",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1295",
   :line 1295,
   :var-type "macro",
   :arglists ([]),
   :doc "Goal that prints the current substitution",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/trace-s"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj",
   :name "waiting-stream-check",
   :file "src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/29917372ef066c42ca362e3a94f68d620ddd1b56/src/main/clojure/clojure/core/logic.clj#L1863",
   :line 1863,
   :var-type "function",
   :arglists ([w success-cont failure-cont]),
   :doc
   "Take a waiting stream, a success continuation, and a failure continuation.\nIf we don't find any ready suspended streams, invoke the failure continuation.\nIf we find a ready suspended stream calculate the remainder of the waiting\nstream. If we've reached the fixpoint just call the thunk of the suspended\nstream, otherwise call mplus on the result of the thunk and the remainder\nof the waiting stream. Pass this result to the success contination.",
   :namespace "clojure.core.logic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/waiting-stream-check"}
  {:name "AnswerCache",
   :var-type "type",
   :namespace "clojure.core.logic",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/AnswerCache",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "Choice",
   :var-type "type",
   :namespace "clojure.core.logic",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/Choice",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "ConstraintStore",
   :var-type "type",
   :namespace "clojure.core.logic",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/ConstraintStore",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "LCons",
   :var-type "type",
   :namespace "clojure.core.logic",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/LCons",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "LVar",
   :var-type "type",
   :namespace "clojure.core.logic",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/LVar",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "PMap",
   :var-type "record",
   :namespace "clojure.core.logic",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/PMap",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "Pair",
   :var-type "type",
   :namespace "clojure.core.logic",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/Pair",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "SubstValue",
   :var-type "record",
   :namespace "clojure.core.logic",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/SubstValue",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "Substitutions",
   :var-type "type",
   :namespace "clojure.core.logic",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/Substitutions",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "SuspendedStream",
   :var-type "record",
   :namespace "clojure.core.logic",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic/SuspendedStream",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj",
   :name "<",
   :file "src/main/clojure/clojure/core/logic/arithmetic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj#L34",
   :line 34,
   :var-type "macro",
   :arglists ([x y]),
   :doc "Goal for testing whether x is less than y. Non-relational.",
   :namespace "clojure.core.logic.arithmetic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.arithmetic/<"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj",
   :name "<=",
   :file "src/main/clojure/clojure/core/logic/arithmetic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj#L43",
   :line 43,
   :var-type "macro",
   :arglists ([x y]),
   :doc
   "Goal for testing whether x is less than or equal to y.\nNon-relational.",
   :namespace "clojure.core.logic.arithmetic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.arithmetic/<="}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj",
   :name "=",
   :file "src/main/clojure/clojure/core/logic/arithmetic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj#L6",
   :line 6,
   :var-type "macro",
   :arglists ([x y]),
   :doc "Goal for testing whether x and y are equal. Non-relational.",
   :namespace "clojure.core.logic.arithmetic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.arithmetic/="}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj",
   :name ">",
   :file "src/main/clojure/clojure/core/logic/arithmetic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj#L15",
   :line 15,
   :var-type "macro",
   :arglists ([x y]),
   :doc
   "Goal for testing whether x is greater than y. Non-relational.",
   :namespace "clojure.core.logic.arithmetic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.arithmetic/>"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj",
   :name ">=",
   :file "src/main/clojure/clojure/core/logic/arithmetic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj#L24",
   :line 24,
   :var-type "macro",
   :arglists ([x y]),
   :doc
   "Goal for testing whether x is greater than or equal to y.\nNon-relational.",
   :namespace "clojure.core.logic.arithmetic",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.arithmetic/>="}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/b256c9ffe3b7fa46850cb3960414571c62293cb7/src/main/clojure/clojure/core/logic/bench.clj",
   :name "all-connected-to-allo",
   :file "src/main/clojure/clojure/core/logic/bench.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/b256c9ffe3b7fa46850cb3960414571c62293cb7/src/main/clojure/clojure/core/logic/bench.clj#L143",
   :line 143,
   :var-type "function",
   :arglists ([l]),
   :doc
   "Collect all cliques in l. l must be bounded to ensure\ntermination.",
   :namespace "clojure.core.logic.bench",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.bench/all-connected-to-allo"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/b256c9ffe3b7fa46850cb3960414571c62293cb7/src/main/clojure/clojure/core/logic/bench.clj",
   :name "connected-to-allo",
   :file "src/main/clojure/clojure/core/logic/bench.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/b256c9ffe3b7fa46850cb3960414571c62293cb7/src/main/clojure/clojure/core/logic/bench.clj#L134",
   :line 134,
   :var-type "function",
   :arglists ([v vs]),
   :doc "Ensure that vertex v is connected to all vertices\nvs.",
   :namespace "clojure.core.logic.bench",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.bench/connected-to-allo"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "!=",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L786",
   :line 786,
   :var-type "function",
   :arglists ([u v]),
   :doc
   "A finite domain constraint. u and v must not be equal. u and v\nmust eventually be given domains if vars.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/!="}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "*",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L976",
   :line 976,
   :var-type "function",
   :arglists ([x y product]),
   :doc
   "A finite domain constraint for multiplication and\nthus division. x, y & product must be eventually be given \ndomains if vars.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/*"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "+",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L900",
   :line 900,
   :var-type "function",
   :arglists ([x y sum]),
   :doc
   "A finite domain constraint for addition and subtraction.\nx, y & sum must eventually be given domains if vars.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/+"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "->FiniteDomain",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L64",
   :line 64,
   :var-type "function",
   :arglists ([s min max]),
   :doc
   "Positional factory function for class clojure.core.logic.fd.FiniteDomain.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/->FiniteDomain"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "->IntervalFD",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L231",
   :line 231,
   :var-type "function",
   :arglists ([lb ub]),
   :doc
   "Positional factory function for class clojure.core.logic.fd.IntervalFD.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/->IntervalFD"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "->MultiIntervalFD",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L477",
   :line 477,
   :var-type "function",
   :arglists ([min max is]),
   :doc
   "Positional factory function for class clojure.core.logic.fd.MultiIntervalFD.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/->MultiIntervalFD"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "-distinctc",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L986",
   :line 986,
   :var-type "function",
   :arglists ([x y* n*]),
   :doc
   "The real *individual* distinct constraint. x is a var that now is bound to\na single value. y* were the non-singleton bound vars that existed at the\nconstruction of the constraint. n* is the set of singleton domain values \nthat existed at the construction of the constraint. We use categorize to \ndetermine the current non-singleton bound vars and singleton vlaues. if x\nis in n* or the new singletons we have failed. If not we simply remove \nthe value of x from the remaining non-singleton domains bound to vars.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/-distinctc"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "<",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L825",
   :line 825,
   :var-type "function",
   :arglists ([u v]),
   :doc
   "A finite domain constraint. u must be less than v. u and v\nmust eventually be given domains if vars.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/<"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "<=",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L819",
   :line 819,
   :var-type "function",
   :arglists ([u v]),
   :doc
   "A finite domain constraint. u must be less than or equal to v.\nu and v must eventually be given domains if vars.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/<="}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "==",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L749",
   :line 749,
   :var-type "function",
   :arglists ([u v]),
   :doc
   "A finite domain constraint. u and v must be equal. u and v must\neventually be given domains if vars.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/=="}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name ">",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L833",
   :line 833,
   :var-type "function",
   :arglists ([u v]),
   :doc
   "A finite domain constraint. u must be greater than v. u and v\nmust eventually be given domains if vars.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/>"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name ">=",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L839",
   :line 839,
   :var-type "function",
   :arglists ([u v]),
   :doc
   "A finite domain constraint. u must be greater than or equal to v.\nu and v must eventually be given domains if vars.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/>="}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "bounded-listo",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L1085",
   :line 1085,
   :var-type "function",
   :arglists ([l n]),
   :doc
   "Ensure that the list l never grows beyond bound n.\nn must have been assigned a domain.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/bounded-listo"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "distinct",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L1077",
   :line 1077,
   :var-type "function",
   :arglists ([v*]),
   :doc
   "A finite domain constraint that will guarantee that \nall vars that occur in v* will be unified with unique \nvalues. v* need not be ground. Any vars in v* should\neventually be given a domain.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/distinct"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "distinctc",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L1042",
   :line 1042,
   :var-type "function",
   :arglists ([v*]),
   :doc
   "The real distinct constraint. v* can be seq of logic vars and\nvalues or it can be a logic var itself. This constraint does not \nrun until v* has become ground. When it has become ground we group\nv* into a set of logic vars and a sorted set of known singleton \nvalues. We then construct the individual constraint for each var.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/distinctc"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "dom",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L631",
   :line 631,
   :var-type "function",
   :arglists ([x dom]),
   :doc "Assign a var x a domain.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/dom"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "domain",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L155",
   :line 155,
   :var-type "function",
   :arglists ([& args]),
   :doc
   "Construct a domain for assignment to a var. Arguments should \nbe integers given in sorted order. domains may be more efficient \nthan intervals when only a few values are possible.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/domain"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "in",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L646",
   :line 646,
   :var-type "macro",
   :arglists ([& xs-and-dom]),
   :doc "Assign vars to domain. The domain must come last.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/in"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "interval",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L357",
   :line 357,
   :var-type "function",
   :arglists ([ub] [lb ub]),
   :doc
   "Construct an interval for an assignment to a var. intervals may\nbe more efficient that the domain type when the range of possiblities\nis large.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/interval"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj",
   :name "process-dom",
   :file "src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/719c23f80280762ff20216a579d88efa32da2de7/src/main/clojure/clojure/core/logic/fd.clj#L616",
   :line 616,
   :var-type "function",
   :arglists ([x dom domp]),
   :doc
   "If x is a var we update its domain. If it's an integer\nwe check that it's a member of the given domain. dom is\nthen new domain, it should have already been calculated from\ndomp which was the previous domain.",
   :namespace "clojure.core.logic.fd",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/process-dom"}
  {:name "FiniteDomain",
   :var-type "type",
   :namespace "clojure.core.logic.fd",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/FiniteDomain",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "IntervalFD",
   :var-type "type",
   :namespace "clojure.core.logic.fd",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/IntervalFD",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "MultiIntervalFD",
   :var-type "type",
   :namespace "clojure.core.logic.fd",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.fd/MultiIntervalFD",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj",
   :name "->Nom",
   :file "src/main/clojure/clojure/core/logic/nominal.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj#L81",
   :line 81,
   :var-type "function",
   :arglists ([lvar]),
   :doc
   "Positional factory function for class clojure.core.logic.nominal.Nom.",
   :namespace "clojure.core.logic.nominal",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.nominal/->Nom"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj",
   :name "->Tie",
   :file "src/main/clojure/clojure/core/logic/nominal.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj#L264",
   :line 264,
   :var-type "function",
   :arglists ([binding-nom body]),
   :doc
   "Positional factory function for class clojure.core.logic.nominal.Tie.",
   :namespace "clojure.core.logic.nominal",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.nominal/->Tie"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj",
   :name "fresh",
   :file "src/main/clojure/clojure/core/logic/nominal.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj#L130",
   :line 130,
   :var-type "macro",
   :arglists ([[& noms] & goals]),
   :doc
   "Creates fresh noms. Goals occuring within form a logical conjunction.",
   :namespace "clojure.core.logic.nominal",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.nominal/fresh"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj",
   :name "map->Tie",
   :file "src/main/clojure/clojure/core/logic/nominal.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj#L264",
   :line 264,
   :var-type "function",
   :arglists ([m#]),
   :doc
   "Factory function for class clojure.core.logic.nominal.Tie, taking a map of keywords to field values.",
   :namespace "clojure.core.logic.nominal",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.nominal/map->Tie"}
  {:name "Nom",
   :var-type "type",
   :namespace "clojure.core.logic.nominal",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.nominal/Nom",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "Tie",
   :var-type "record",
   :namespace "clojure.core.logic.nominal",
   :arglists nil,
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.nominal/Tie",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/dba7c697ca45a7fee7595cd18d057c420261d498/src/main/clojure/clojure/core/logic/unifier.clj",
   :name "prep",
   :file "src/main/clojure/clojure/core/logic/unifier.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/dba7c697ca45a7fee7595cd18d057c420261d498/src/main/clojure/clojure/core/logic/unifier.clj#L54",
   :line 54,
   :var-type "function",
   :arglists ([expr]),
   :doc
   "Prep a quoted expression. All symbols preceded by ? will\nbe replaced with logic vars.",
   :namespace "clojure.core.logic.unifier",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.unifier/prep"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/dba7c697ca45a7fee7595cd18d057c420261d498/src/main/clojure/clojure/core/logic/unifier.clj",
   :name "unifier",
   :file "src/main/clojure/clojure/core/logic/unifier.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/dba7c697ca45a7fee7595cd18d057c420261d498/src/main/clojure/clojure/core/logic/unifier.clj#L147",
   :line 147,
   :var-type "function",
   :arglists ([ts] [opts ts]),
   :doc "Return the unifier for terms ts. Will prep the terms.",
   :namespace "clojure.core.logic.unifier",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.unifier/unifier"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/dba7c697ca45a7fee7595cd18d057c420261d498/src/main/clojure/clojure/core/logic/unifier.clj",
   :name "unifier*",
   :file "src/main/clojure/clojure/core/logic/unifier.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/dba7c697ca45a7fee7595cd18d057c420261d498/src/main/clojure/clojure/core/logic/unifier.clj#L118",
   :line 118,
   :var-type "function",
   :arglists ([ts] [opts ts]),
   :doc
   "Return the unifier that unifies terms ts.\nAll terms in ts should prepped terms.",
   :namespace "clojure.core.logic.unifier",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.unifier/unifier*"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/dba7c697ca45a7fee7595cd18d057c420261d498/src/main/clojure/clojure/core/logic/unifier.clj",
   :name "unify",
   :file "src/main/clojure/clojure/core/logic/unifier.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/dba7c697ca45a7fee7595cd18d057c420261d498/src/main/clojure/clojure/core/logic/unifier.clj#L134",
   :line 134,
   :var-type "function",
   :arglists ([ts] [opts ts]),
   :doc
   "Unify the terms ts returning a the value that represents their\nunificaiton. Will prep the terms.",
   :namespace "clojure.core.logic.unifier",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.unifier/unify"}
  {:raw-source-url
   "https://github.com/clojure/core.logic/raw/dba7c697ca45a7fee7595cd18d057c420261d498/src/main/clojure/clojure/core/logic/unifier.clj",
   :name "unify*",
   :file "src/main/clojure/clojure/core/logic/unifier.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/dba7c697ca45a7fee7595cd18d057c420261d498/src/main/clojure/clojure/core/logic/unifier.clj#L108",
   :line 108,
   :var-type "function",
   :arglists ([ts] [opts ts]),
   :doc "Unify the terms ts.",
   :namespace "clojure.core.logic.unifier",
   :wiki-url
   "http://clojure.github.io/core.logic//index.html#clojure.core.logic.unifier/unify*"})}
