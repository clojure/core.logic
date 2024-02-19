core.logic
====

A logic programming library for Clojure & ClojureScript. core.logic
offers Prolog-like relational programming, constraint logic
programming, and nominal logic programming for Clojure. At its heart
is an original implementation of miniKanren as described in William
Byrd's dissertation
[Relational Programming in miniKanren: Techniques, Applications, and Implementations](https://www.proquest.com/docview/304903505/E30282E6EF13453CPQ/1)
as well as the extensions described in
[cKanren](https://www.schemeworkshop.org/2011/papers/Alvis2011.pdf) and
[αKanren](http://webyrd.net/alphamk/alphamk.pdf). It
is designed to be easily extended to forms of logic programming beyond
the ones provided.

Releases and dependency information
----

This project follows the version scheme MAJOR.MINOR.PATCH where each component provides some relative indication of the size of the change, but does not follow semantic versioning. In general, all changes endeavor to be non-breaking (by moving to new names rather than by breaking existing names).

Latest stable release: 1.1.0

* [All released versions](https://search.maven.org/#search%7Cgav%7C1%7Cg%3A%22org.clojure%22%20AND%20a%3A%22core.logic%22)
* [Development snapshot version](https://oss.sonatype.org/index.html#nexus-search;gav~org.clojure~core.logic~~~)

[CLI/`deps.edn`](https://clojure.org/reference/deps_and_cli) dependency information:
```clojure
org.clojure/core.logic {:mvn/version "1.1.0"}
```

[Leiningen](https://github.com/technomancy/leiningen/) dependency information:

```
[org.clojure/core.logic "1.1.0"]
```

[Maven](https://maven.apache.org) dependency information:

```
<dependency>
  <groupId>org.clojure</groupId>
  <artifactId>core.logic</artifactId>
  <version>1.1.0</version>
</dependency>
```

Example usage
----

```clojure
(use 'clojure.core.logic)

(run* [q]
  (== q true))  
;;=> (true)
```

Running the tests
----

Assuming you have V8 installed from source:

```
lein cljsbuild once adv
d8 resources/tests.js
```

If you have another JS engine installed use that instead.

Reasoned Schemer
----

If you wish to work through
[The Reasoned Schemer](https://mitpress.mit.edu/books/reasoned-schemer-second-edition) with
core.logic make sure to look over
[this](https://github.com/clojure/core.logic/wiki/Differences-from-The-Reasoned-Schemer)
first.

If you're interested in using core.logic from
[ClojureScript](https://github.com/clojure/clojurescript/) look
[here](https://github.com/clojure/core.logic/wiki/Using-core.logic-with-ClojureScript).

For more information & documentation please consult the
[wiki](https://github.com/clojure/core.logic/wiki).

Differences from core.unify
----

[core.unify](https://github.com/clojure/core.unify) provides a la carte
unification facilities that are not deeply tied into the operation of
a logic engine. While core.logic does provide a similar simple unifier
interface with support for specifying fine-grained constraints, if you
have no need for a logic programming system, core.unify may be a
better fit.

YourKit
----

<img src="https://www.yourkit.com/images/yklogo.png"></img>

YourKit has given an open source license for their profiler, greatly
simplifying the profiling of core.logic performance.

YourKit supports open source projects with its full-featured Java
Profiler.  YourKit, LLC is the creator of <a
href="https://www.yourkit.com/java/profiler/index.jsp">YourKit Java
Profiler</a> and <a
href="https://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET
Profiler</a>, innovative and intelligent tools for profiling Java and
.NET applications.

Developer information
----

* [Bug Tracker](https://clojure.atlassian.net/browse/LOGIC)
* [Continuous Integration](https://github.com/clojure/core.logic/actions/workflows/test.yml)

Copyright and license
----

Copyright © David Nolen, Rich Hickey & contributors.

Licensed under the EPL (see the file epl.html).
