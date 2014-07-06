{:namespaces
 ({:source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic/clojure.core.logic-api.html",
   :name "clojure.core.logic",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.logic/blob/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic/clojure.core.logic.arithmetic-api.html",
   :name "clojure.core.logic.arithmetic",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.logic/blob/b256c9ffe3b7fa46850cb3960414571c62293cb7/src/main/clojure/clojure/core/logic/bench.clj",
   :wiki-url
   "http://clojure.github.com/core.logic/clojure.core.logic.bench-api.html",
   :name "clojure.core.logic.bench",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic/clojure.core.logic.fd-api.html",
   :name "clojure.core.logic.fd",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj",
   :wiki-url
   "http://clojure.github.com/core.logic/clojure.core.logic.nominal-api.html",
   :name "clojure.core.logic.nominal",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/unifier.clj",
   :wiki-url
   "http://clojure.github.com/core.logic/clojure.core.logic.unifier-api.html",
   :name "clojure.core.logic.unifier",
   :doc nil}),
 :vars
 ({:arglists ([u v]),
   :name "!=",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L2437",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/!=",
   :doc
   "Disequality constraint. Ensures that u and v will never\nunify. u and v can be complex terms.",
   :var-type "function",
   :line 2437,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([ansl anss _meta]),
   :name "->AnswerCache",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1797",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/->AnswerCache",
   :doc
   "Positional factory function for class clojure.core.logic.AnswerCache.",
   :var-type "function",
   :line 1797,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([a f]),
   :name "->Choice",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1084",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/->Choice",
   :doc
   "Positional factory function for class clojure.core.logic.Choice.",
   :var-type "function",
   :line 1084,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([km cm cid running]),
   :name "->ConstraintStore",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L109",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/->ConstraintStore",
   :doc
   "Positional factory function for class clojure.core.logic.ConstraintStore.",
   :var-type "function",
   :line 109,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([a d cache meta]),
   :name "->LCons",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L755",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/->LCons",
   :doc
   "Positional factory function for class clojure.core.logic.LCons.",
   :var-type "function",
   :line 755,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([id unique name oname hash meta]),
   :name "->LVar",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L621",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/->LVar",
   :doc
   "Positional factory function for class clojure.core.logic.LVar.",
   :var-type "function",
   :line 621,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([]),
   :name "->PMap",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L2497",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/->PMap",
   :doc
   "Positional factory function for class clojure.core.logic.PMap.",
   :var-type "function",
   :line 2497,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([lhs rhs]),
   :name "->Pair",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L43",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/->Pair",
   :doc
   "Positional factory function for class clojure.core.logic.Pair.",
   :var-type "function",
   :line 43,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([v doms eset]),
   :name "->SubstValue",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L197",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/->SubstValue",
   :doc
   "Positional factory function for class clojure.core.logic.SubstValue.",
   :var-type "function",
   :line 197,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([s vs ts cs cq cqs oc _meta]),
   :name "->Substitutions",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L287",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/->Substitutions",
   :doc
   "Positional factory function for class clojure.core.logic.Substitutions.",
   :var-type "function",
   :line 287,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([cache ansv* f]),
   :name "->SuspendedStream",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1828",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/->SuspendedStream",
   :doc
   "Positional factory function for class clojure.core.logic.SuspendedStream.",
   :var-type "function",
   :line 1828,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([u v]),
   :name "==",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1156",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/==",
   :doc "A goal that attempts to unify terms u and v.",
   :var-type "function",
   :line 1156,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([] [& goals]),
   :name "all",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1253",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/all",
   :doc "Like fresh but does does not create logic variables.",
   :var-type "macro",
   :line 1253,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:file "src/main/clojure/clojure/core/logic.clj",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1751",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/appendo",
   :namespace "clojure.core.logic",
   :line 1751,
   :var-type "var",
   :doc
   "A relation where x, y, and z are proper collections,\nsuch that z is x appended to y",
   :name "appendo"}
  {:arglists ([& clauses]),
   :name "conda",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1396",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/conda",
   :doc
   "Soft cut. Once the head of a clause has succeeded\nall other clauses will be ignored. Non-relational.",
   :var-type "macro",
   :line 1396,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([& clauses]),
   :name "conde",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1175",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/conde",
   :doc
   "Logical disjunction of the clauses. The first goal in\na clause is considered the head of that clause. Interleaves the\nexecution of the clauses.",
   :var-type "macro",
   :line 1175,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([& clauses]),
   :name "condu",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1404",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/condu",
   :doc
   "Committed choice. Once the head (first goal) of a clause\nhas succeeded, remaining goals of the clause will only\nbe run once. Non-relational.",
   :var-type "macro",
   :line 1404,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([coll & args]),
   :name "conjo",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L2781",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/conjo",
   :doc "A constraint version of conj",
   :var-type "function",
   :line 2781,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([a d l]),
   :name "conso",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1638",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/conso",
   :doc
   "A relation where l is a collection, such that a is the first of l\nand d is the rest of l. If ground d must be bound to a proper tail.",
   :var-type "function",
   :line 1638,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([u v]),
   :name "copy-term",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1418",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/copy-term",
   :doc "Copies a term u into v. Non-relational.",
   :var-type "function",
   :line 1418,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([& rest]),
   :name "defna",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1707",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/defna",
   :doc "Define a soft cut goal. See conda.",
   :var-type "macro",
   :line 1707,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([& rest]),
   :name "defne",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1678",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/defne",
   :doc
   "Define a goal fn. Supports pattern matching. All\npatterns will be tried. See conde.",
   :var-type "macro",
   :line 1678,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([& rest]),
   :name "defnu",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1712",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/defnu",
   :doc "Define a committed choice goal. See condu.",
   :var-type "macro",
   :line 1712,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:file "src/main/clojure/clojure/core/logic.clj",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L2451",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/distincto",
   :namespace "clojure.core.logic",
   :line 2451,
   :var-type "var",
   :doc
   "A relation which guarantees no element of l will unify\nwith another element of l.",
   :name "distincto"}
  {:arglists ([a]),
   :name "emptyo",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1633",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/emptyo",
   :doc "A relation where a is the empty list",
   :var-type "function",
   :line 1633,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([g coll]),
   :name "everyg",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1656",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/everyg",
   :doc
   "A pseudo-relation that takes a coll and ensures that the goal g\nsucceeds on every element of the collection.",
   :var-type "function",
   :line 1656,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([a]),
   :name "fail",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1144",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/fail",
   :doc "A goal that always fails.",
   :var-type "function",
   :line 1144,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([x fs]),
   :name "featurec",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L2558",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/featurec",
   :doc
   "Ensure that a map contains at least the key-value pairs\nin the map fs. fs must be partially instantiated - that is,\nit may contain values which are logic variables to support\nfeature extraction.",
   :var-type "function",
   :line 2558,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([l a]),
   :name "firsto",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1644",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/firsto",
   :doc
   "A relation where l is a collection, such that a is the first of l",
   :var-type "function",
   :line 1644,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([a]),
   :name "fix-constraints",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L2067",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/fix-constraints",
   :doc
   "A goal to run the constraints in cq until it is empty. Of\ncourse running a constraint may grow cq so this function\nfinds the fixpoint.",
   :var-type "function",
   :line 2067,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([& rest]),
   :name "fna",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1697",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/fna",
   :doc "Define an anonymous soft cut goal. See conda.",
   :var-type "macro",
   :line 1697,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([args & body]),
   :name "fnc",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L2590",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/fnc",
   :doc
   "Define an anonymous constraint that can be used with the unifier:\n\n   (let [oddc (fnc [x] (odd? x))]\n\n     (unifier {:a '?a} {:a 1} :when {'?a oddc})\n       ;;=> {:a 1}\n\n     (unifier {:a '?a} {:a 2} :when {'?a oddc})\n       ;;=> nil\n   )\n\nNote, the constraint will not run until all arguments are fully ground.\n\nUse defnc to define a constraint and assign a toplevel var.",
   :var-type "macro",
   :line 2590,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([& rest]),
   :name "fne",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1672",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/fne",
   :doc
   "Define an anonymous goal fn. Supports pattern matching. All\npatterns will be tried. See conde.",
   :var-type "macro",
   :line 1672,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([& rest]),
   :name "fnu",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1702",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/fnu",
   :doc "Define an anonymous committed choice goal. See condu.",
   :var-type "macro",
   :line 1702,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([[& lvars] & goals]),
   :name "fresh",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1192",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/fresh",
   :doc
   "Creates fresh variables. Goals occuring within form a logical\nconjunction.",
   :var-type "macro",
   :line 1192,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([u v op]),
   :name "is",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1323",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/is",
   :doc
   "Set the value of a var to value of another var with the operation\napplied. Non-relational.",
   :var-type "macro",
   :line 1323,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([a d]),
   :name "lcons",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L868",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/lcons",
   :doc
   "Constructs a sequence a with an improper tail d if d is a logic variable.",
   :var-type "function",
   :line 868,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([f s] [f s & rest]),
   :name "llist",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L878",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/llist",
   :doc
   "Constructs a sequence from 2 or more arguments, with the last argument as the\ntail. The tail is improper if the last argument is a logic variable.",
   :var-type "macro",
   :line 878,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([& s]),
   :name "log",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1267",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/log",
   :doc "Goal for println",
   :var-type "macro",
   :line 1267,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([v]),
   :name "lvaro",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1427",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/lvaro",
   :doc
   "A goal that succeeds if the argument is fresh. v must be a logic\nvariable. Non-relational.",
   :var-type "macro",
   :line 1427,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([m__5869__auto__]),
   :name "map->PMap",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L2497",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/map->PMap",
   :doc
   "Factory function for class clojure.core.logic.PMap, taking a map of keywords to field values.",
   :var-type "function",
   :line 2497,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([m__5869__auto__]),
   :name "map->SubstValue",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L197",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/map->SubstValue",
   :doc
   "Factory function for class clojure.core.logic.SubstValue, taking a map of keywords to field values.",
   :var-type "function",
   :line 197,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([m__5869__auto__]),
   :name "map->SuspendedStream",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1828",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/map->SuspendedStream",
   :doc
   "Factory function for class clojure.core.logic.SuspendedStream, taking a map of keywords to field values.",
   :var-type "function",
   :line 1828,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([argv cache]),
   :name "master",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1955",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/master",
   :doc
   "Take the argument to the goal and check that we don't\nhave an alpha equivalent cached answer term in the cache.\nIf it doesn't already exist in the cache add the new\nanswer term.",
   :var-type "function",
   :line 1955,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([xs & cs]),
   :name "matcha",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1717",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/matcha",
   :doc "Define a soft cut pattern match. See conda.",
   :var-type "macro",
   :line 1717,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([xs & cs]),
   :name "matche",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1684",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/matche",
   :doc
   "Pattern matching macro. All patterns will be tried.\nSee conde.",
   :var-type "macro",
   :line 1684,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([xs & cs]),
   :name "matchu",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1723",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/matchu",
   :doc "Define a committed choice goal. See condu.",
   :var-type "macro",
   :line 1723,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:file "src/main/clojure/clojure/core/logic.clj",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1741",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/member1o",
   :namespace "clojure.core.logic",
   :line 1741,
   :var-type "var",
   :doc
   "Like membero but uses to disequality further constraining\nthe results. For example, if x and l are ground and x occurs\nmultiple times in l, member1o will succeed only once.",
   :name "member1o"}
  {:file "src/main/clojure/clojure/core/logic.clj",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1734",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/membero",
   :namespace "clojure.core.logic",
   :line 1734,
   :var-type "var",
   :doc "A relation where l is a collection, such that l contains x.",
   :name "membero"}
  {:arglists ([c & args]),
   :name "nafc",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L2704",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/nafc",
   :doc
   "EXPERIMENTAL: negation as failure constraint. All arguments to the goal c\nmust be ground. If some argument is not ground the execution of this constraint\nwill be delayed.",
   :var-type "function",
   :line 2704,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([a]),
   :name "nilo",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1628",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/nilo",
   :doc "A relation where a is nil",
   :var-type "function",
   :line 1628,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([v]),
   :name "nonlvaro",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1435",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/nonlvaro",
   :doc
   "A goal that succeeds if the argument is not fresh. v must be a\nlogic variable. Non-relational.",
   :var-type "macro",
   :line 1435,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([m]),
   :name "partial-map",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L2515",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/partial-map",
   :doc
   "Given map m, returns partial map that unifies with maps even if it\ndoesn't share all of the keys of that map.",
   :var-type "function",
   :line 2515,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:file "src/main/clojure/clojure/core/logic.clj",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1760",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/permuteo",
   :namespace "clojure.core.logic",
   :line 1760,
   :var-type "var",
   :doc
   "A relation that will permute xl into the yl. May not\nterminate if xl is not ground.",
   :name "permuteo"}
  {:arglists ([v f]),
   :name "pred",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1315",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/pred",
   :doc
   "Check a predicate against the value logic var. Non-relational.",
   :var-type "macro",
   :line 1315,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([[& vars] & goals]),
   :name "project",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1306",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/project",
   :doc
   "Extract the values bound to the specified logic vars. Non-relational.",
   :var-type "macro",
   :line 1306,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:file "src/main/clojure/clojure/core/logic.clj",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L2462",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/rembero",
   :namespace "clojure.core.logic",
   :line 2462,
   :var-type "var",
   :doc
   "A relation between l and o where x is removed from\nl exactly one time.",
   :name "rembero"}
  {:arglists ([l d]),
   :name "resto",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1650",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/resto",
   :doc
   "A relation where l is a collection, such that d is the rest of l",
   :var-type "function",
   :line 1650,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([n bindings & goals]),
   :name "run",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1222",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/run",
   :doc "Executes goals until a maximum of n results are found.",
   :var-type "macro",
   :line 1222,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([bindings & goals]),
   :name "run*",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1227",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/run*",
   :doc "Executes goals until results are exhausted.",
   :var-type "macro",
   :line 1227,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([n db bindings & goals]),
   :name "run-db",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1232",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/run-db",
   :doc
   "Executes goals until a maximum of n results are found. Uses a specified logic database.",
   :var-type "macro",
   :line 1232,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([db bindings & goals]),
   :name "run-db*",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1237",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/run-db*",
   :doc
   "Executes goals until results are exhausted. Uses a specified logic database.",
   :var-type "macro",
   :line 1237,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([n bindings & goals]),
   :name "run-nc",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1242",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/run-nc",
   :doc
   "Executes goals until a maximum of n results are found. Does not\noccurs-check.",
   :var-type "macro",
   :line 1242,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([& goals]),
   :name "run-nc*",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1248",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/run-nc*",
   :doc
   "Executes goals until results are exhausted. Does not occurs-check.",
   :var-type "macro",
   :line 1248,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([a]),
   :name "succeed",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1140",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/succeed",
   :doc "A goal that always succeeds.",
   :var-type "function",
   :line 1140,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([args & grest]),
   :name "tabled",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1976",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/tabled",
   :doc
   "Macro for defining a tabled goal. Prefer ^:tabled with the\ndefne/a/u forms over using this directly.",
   :var-type "macro",
   :line 1976,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([title & lvars]),
   :name "trace-lvars",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1284",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/trace-lvars",
   :doc "Goal for tracing the values of logic variables.",
   :var-type "macro",
   :line 1284,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([]),
   :name "trace-s",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1274",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/trace-s",
   :doc "Goal that prints the current substitution",
   :var-type "macro",
   :line 1274,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:arglists ([w success-cont failure-cont]),
   :name "waiting-stream-check",
   :namespace "clojure.core.logic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj#L1842",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5221af81fcc6d391c7db5841807ac2daa14926d6/src/main/clojure/clojure/core/logic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/waiting-stream-check",
   :doc
   "Take a waiting stream, a success continuation, and a failure continuation.\nIf we don't find any ready suspended streams, invoke the failure continuation.\nIf we find a ready suspended stream calculate the remainder of the waiting\nstream. If we've reached the fixpoint just call the thunk of the suspended\nstream, otherwise call mplus on the result of the thunk and the remainder\nof the waiting stream. Pass this result to the success contination.",
   :var-type "function",
   :line 1842,
   :file "src/main/clojure/clojure/core/logic.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/AnswerCache",
   :namespace "clojure.core.logic",
   :var-type "type",
   :name "AnswerCache"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/Choice",
   :namespace "clojure.core.logic",
   :var-type "type",
   :name "Choice"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/ConstraintStore",
   :namespace "clojure.core.logic",
   :var-type "type",
   :name "ConstraintStore"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/LCons",
   :namespace "clojure.core.logic",
   :var-type "type",
   :name "LCons"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/LVar",
   :namespace "clojure.core.logic",
   :var-type "type",
   :name "LVar"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/PMap",
   :namespace "clojure.core.logic",
   :var-type "record",
   :name "PMap"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/Pair",
   :namespace "clojure.core.logic",
   :var-type "type",
   :name "Pair"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/SubstValue",
   :namespace "clojure.core.logic",
   :var-type "record",
   :name "SubstValue"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/Substitutions",
   :namespace "clojure.core.logic",
   :var-type "type",
   :name "Substitutions"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic/SuspendedStream",
   :namespace "clojure.core.logic",
   :var-type "record",
   :name "SuspendedStream"}
  {:arglists ([x y]),
   :name "<",
   :namespace "clojure.core.logic.arithmetic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj#L34",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.arithmetic/<",
   :doc "Goal for testing whether x is less than y. Non-relational.",
   :var-type "macro",
   :line 34,
   :file "src/main/clojure/clojure/core/logic/arithmetic.clj"}
  {:arglists ([x y]),
   :name "<=",
   :namespace "clojure.core.logic.arithmetic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj#L43",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.arithmetic/<=",
   :doc
   "Goal for testing whether x is less than or equal to y.\nNon-relational.",
   :var-type "macro",
   :line 43,
   :file "src/main/clojure/clojure/core/logic/arithmetic.clj"}
  {:arglists ([x y]),
   :name "=",
   :namespace "clojure.core.logic.arithmetic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj#L6",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.arithmetic/=",
   :doc "Goal for testing whether x and y are equal. Non-relational.",
   :var-type "macro",
   :line 6,
   :file "src/main/clojure/clojure/core/logic/arithmetic.clj"}
  {:arglists ([x y]),
   :name ">",
   :namespace "clojure.core.logic.arithmetic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj#L15",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.arithmetic/>",
   :doc
   "Goal for testing whether x is greater than y. Non-relational.",
   :var-type "macro",
   :line 15,
   :file "src/main/clojure/clojure/core/logic/arithmetic.clj"}
  {:arglists ([x y]),
   :name ">=",
   :namespace "clojure.core.logic.arithmetic",
   :source-url
   "https://github.com/clojure/core.logic/blob/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj#L24",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/5bf5147afdb0fe04da658c3573ca63a922975d05/src/main/clojure/clojure/core/logic/arithmetic.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.arithmetic/>=",
   :doc
   "Goal for testing whether x is greater than or equal to y.\nNon-relational.",
   :var-type "macro",
   :line 24,
   :file "src/main/clojure/clojure/core/logic/arithmetic.clj"}
  {:file "src/main/clojure/clojure/core/logic/bench.clj",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/b256c9ffe3b7fa46850cb3960414571c62293cb7/src/main/clojure/clojure/core/logic/bench.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/b256c9ffe3b7fa46850cb3960414571c62293cb7/src/main/clojure/clojure/core/logic/bench.clj#L143",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.bench/all-connected-to-allo",
   :namespace "clojure.core.logic.bench",
   :line 143,
   :var-type "var",
   :doc
   "Collect all cliques in l. l must be bounded to ensure\ntermination.",
   :name "all-connected-to-allo"}
  {:file "src/main/clojure/clojure/core/logic/bench.clj",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/b256c9ffe3b7fa46850cb3960414571c62293cb7/src/main/clojure/clojure/core/logic/bench.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/b256c9ffe3b7fa46850cb3960414571c62293cb7/src/main/clojure/clojure/core/logic/bench.clj#L134",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.bench/connected-to-allo",
   :namespace "clojure.core.logic.bench",
   :line 134,
   :var-type "var",
   :doc "Ensure that vertex v is connected to all vertices\nvs.",
   :name "connected-to-allo"}
  {:arglists ([u v]),
   :name "!=",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L783",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/!=",
   :doc
   "A finite domain constraint. u and v must not be equal. u and v\nmust eventually be given domains if vars.",
   :var-type "function",
   :line 783,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([x y product]),
   :name "*",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L973",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/*",
   :doc
   "A finite domain constraint for multiplication and\nthus division. x, y & product must be eventually be given \ndomains if vars.",
   :var-type "function",
   :line 973,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([x y sum]),
   :name "+",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L897",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/+",
   :doc
   "A finite domain constraint for addition and subtraction.\nx, y & sum must eventually be given domains if vars.",
   :var-type "function",
   :line 897,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([s min max]),
   :name "->FiniteDomain",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L64",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/->FiniteDomain",
   :doc
   "Positional factory function for class clojure.core.logic.fd.FiniteDomain.",
   :var-type "function",
   :line 64,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([lb ub]),
   :name "->IntervalFD",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L230",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/->IntervalFD",
   :doc
   "Positional factory function for class clojure.core.logic.fd.IntervalFD.",
   :var-type "function",
   :line 230,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([min max is]),
   :name "->MultiIntervalFD",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L474",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/->MultiIntervalFD",
   :doc
   "Positional factory function for class clojure.core.logic.fd.MultiIntervalFD.",
   :var-type "function",
   :line 474,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([x y* n*]),
   :name "-distinctc",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L983",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/-distinctc",
   :doc
   "The real *individual* distinct constraint. x is a var that now is bound to\na single value. y* were the non-singleton bound vars that existed at the\nconstruction of the constraint. n* is the set of singleton domain values \nthat existed at the construction of the constraint. We use categorize to \ndetermine the current non-singleton bound vars and singleton vlaues. if x\nis in n* or the new singletons we have failed. If not we simply remove \nthe value of x from the remaining non-singleton domains bound to vars.",
   :var-type "function",
   :line 983,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([u v]),
   :name "<",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L822",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/<",
   :doc
   "A finite domain constraint. u must be less than v. u and v\nmust eventually be given domains if vars.",
   :var-type "function",
   :line 822,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([u v]),
   :name "<=",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L816",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/<=",
   :doc
   "A finite domain constraint. u must be less than or equal to v.\nu and v must eventually be given domains if vars.",
   :var-type "function",
   :line 816,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([u v]),
   :name "==",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L746",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/==",
   :doc
   "A finite domain constraint. u and v must be equal. u and v must\neventually be given domains if vars.",
   :var-type "function",
   :line 746,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([u v]),
   :name ">",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L830",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/>",
   :doc
   "A finite domain constraint. u must be greater than v. u and v\nmust eventually be given domains if vars.",
   :var-type "function",
   :line 830,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([u v]),
   :name ">=",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L836",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/>=",
   :doc
   "A finite domain constraint. u must be greater than or equal to v.\nu and v must eventually be given domains if vars.",
   :var-type "function",
   :line 836,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:file "src/main/clojure/clojure/core/logic/fd.clj",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L1082",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/bounded-listo",
   :namespace "clojure.core.logic.fd",
   :line 1082,
   :var-type "var",
   :doc
   "Ensure that the list l never grows beyond bound n.\nn must have been assigned a domain.",
   :name "bounded-listo"}
  {:arglists ([v*]),
   :name "distinct",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L1074",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/distinct",
   :doc
   "A finite domain constraint that will guarantee that \nall vars that occur in v* will be unified with unique \nvalues. v* need not be ground. Any vars in v* should\neventually be given a domain.",
   :var-type "function",
   :line 1074,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([v*]),
   :name "distinctc",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L1039",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/distinctc",
   :doc
   "The real distinct constraint. v* can be seq of logic vars and\nvalues or it can be a logic var itself. This constraint does not \nrun until v* has become ground. When it has become ground we group\nv* into a set of logic vars and a sorted set of known singleton \nvalues. We then construct the individual constraint for each var.",
   :var-type "function",
   :line 1039,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([x dom]),
   :name "dom",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L628",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/dom",
   :doc "Assign a var x a domain.",
   :var-type "function",
   :line 628,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([& args]),
   :name "domain",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L155",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/domain",
   :doc
   "Construct a domain for assignment to a var. Arguments should \nbe integers given in sorted order. domains may be more efficient \nthan intervals when only a few values are possible.",
   :var-type "function",
   :line 155,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([& xs-and-dom]),
   :name "in",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L643",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/in",
   :doc "Assign vars to domain. The domain must come last.",
   :var-type "macro",
   :line 643,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([ub] [lb ub]),
   :name "interval",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L356",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/interval",
   :doc
   "Construct an interval for an assignment to a var. intervals may\nbe more efficient that the domain type when the range of possiblities\nis large.",
   :var-type "function",
   :line 356,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:arglists ([x dom domp]),
   :name "process-dom",
   :namespace "clojure.core.logic.fd",
   :source-url
   "https://github.com/clojure/core.logic/blob/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj#L613",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/9b9b3a8bb87ab5ccac2491fb5fdd80af18c47847/src/main/clojure/clojure/core/logic/fd.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/process-dom",
   :doc
   "If x is a var we update its domain. If it's an integer\nwe check that it's a member of the given domain. dom is\nthen new domain, it should have already been calculated from\ndomp which was the previous domain.",
   :var-type "function",
   :line 613,
   :file "src/main/clojure/clojure/core/logic/fd.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/FiniteDomain",
   :namespace "clojure.core.logic.fd",
   :var-type "type",
   :name "FiniteDomain"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/IntervalFD",
   :namespace "clojure.core.logic.fd",
   :var-type "type",
   :name "IntervalFD"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.fd/MultiIntervalFD",
   :namespace "clojure.core.logic.fd",
   :var-type "type",
   :name "MultiIntervalFD"}
  {:arglists ([lvar]),
   :name "->Nom",
   :namespace "clojure.core.logic.nominal",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj#L81",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.nominal/->Nom",
   :doc
   "Positional factory function for class clojure.core.logic.nominal.Nom.",
   :var-type "function",
   :line 81,
   :file "src/main/clojure/clojure/core/logic/nominal.clj"}
  {:arglists ([binding-nom body]),
   :name "->Tie",
   :namespace "clojure.core.logic.nominal",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj#L264",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.nominal/->Tie",
   :doc
   "Positional factory function for class clojure.core.logic.nominal.Tie.",
   :var-type "function",
   :line 264,
   :file "src/main/clojure/clojure/core/logic/nominal.clj"}
  {:arglists ([[& noms] & goals]),
   :name "fresh",
   :namespace "clojure.core.logic.nominal",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj#L130",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.nominal/fresh",
   :doc
   "Creates fresh noms. Goals occuring within form a logical conjunction.",
   :var-type "macro",
   :line 130,
   :file "src/main/clojure/clojure/core/logic/nominal.clj"}
  {:arglists ([m__5869__auto__]),
   :name "map->Tie",
   :namespace "clojure.core.logic.nominal",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj#L264",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/nominal.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.nominal/map->Tie",
   :doc
   "Factory function for class clojure.core.logic.nominal.Tie, taking a map of keywords to field values.",
   :var-type "function",
   :line 264,
   :file "src/main/clojure/clojure/core/logic/nominal.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.nominal/Nom",
   :namespace "clojure.core.logic.nominal",
   :var-type "type",
   :name "Nom"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.nominal/Tie",
   :namespace "clojure.core.logic.nominal",
   :var-type "record",
   :name "Tie"}
  {:arglists ([expr]),
   :name "prep",
   :namespace "clojure.core.logic.unifier",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/unifier.clj#L55",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/unifier.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.unifier/prep",
   :doc
   "Prep a quoted expression. All symbols preceded by ? will\nbe replaced with logic vars.",
   :var-type "function",
   :line 55,
   :file "src/main/clojure/clojure/core/logic/unifier.clj"}
  {:arglists ([ts] [opts ts]),
   :name "unifier",
   :namespace "clojure.core.logic.unifier",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/unifier.clj#L148",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/unifier.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.unifier/unifier",
   :doc "Return the unifier for terms ts. Will prep the terms.",
   :var-type "function",
   :line 148,
   :file "src/main/clojure/clojure/core/logic/unifier.clj"}
  {:arglists ([ts] [opts ts]),
   :name "unifier*",
   :namespace "clojure.core.logic.unifier",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/unifier.clj#L119",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/unifier.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.unifier/unifier*",
   :doc
   "Return the unifier that unifies terms ts.\nAll terms in ts should prepped terms.",
   :var-type "function",
   :line 119,
   :file "src/main/clojure/clojure/core/logic/unifier.clj"}
  {:arglists ([ts] [opts ts]),
   :name "unify",
   :namespace "clojure.core.logic.unifier",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/unifier.clj#L135",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/unifier.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.unifier/unify",
   :doc
   "Unify the terms ts returning a the value that represents their\nunificaiton. Will prep the terms.",
   :var-type "function",
   :line 135,
   :file "src/main/clojure/clojure/core/logic/unifier.clj"}
  {:arglists ([ts] [opts ts]),
   :name "unify*",
   :namespace "clojure.core.logic.unifier",
   :source-url
   "https://github.com/clojure/core.logic/blob/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/unifier.clj#L109",
   :raw-source-url
   "https://github.com/clojure/core.logic/raw/f329cfc6a9544859629275a59c30cef0ab3cedea/src/main/clojure/clojure/core/logic/unifier.clj",
   :wiki-url
   "http://clojure.github.com/core.logic//clojure.core.logic-api.html#clojure.core.logic.unifier/unify*",
   :doc "Unify the terms ts.",
   :var-type "function",
   :line 109,
   :file "src/main/clojure/clojure/core/logic/unifier.clj"})}
