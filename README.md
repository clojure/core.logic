core.logic
====

A logic programming library for Clojure & ClojureScript. core.logic
offers Prolog-like relational programming, constraint logic
programming, and nominal logic programming for Clojure. At its heart
is an original implementation of miniKanren as described in William
Byrd's dissertation
[Relational Programming in miniKanren: Techniques, Applications, and Implementations](http://pqdtopen.proquest.com/#abstract?dispub=3380156)
as well as the extensions described in
[cKanren](http://www.schemeworkshop.org/2011/papers/Alvis2011.pdf) and
[αKanren](http://www.cs.indiana.edu/~webyrd/alphamk/alphamk.pdf). It
is designed to be easily extended to forms of logic programming beyond
the ones provided.

Releases and dependency information
----

Latest stable release: 0.8.11

* [All released versions](http://search.maven.org/#search%7Cgav%7C1%7Cg%3A%22org.clojure%22%20AND%20a%3A%22core.logic%22)
* [Development snapshot version](http://oss.sonatype.org/index.html#nexus-search;gav~org.clojure~core.logic~~~)

[Leiningen](http://github.com/technomancy/leiningen/) dependency information:

```
[org.clojure/core.logic "0.8.11"]
```

[Maven](http://maven.apache.org) dependency information:

```
<dependency>
  <groupId>org.clojure</groupId>
  <artifactId>core.logic</artifactId>
  <version>0.8.11</version>
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
[The Reasoned Schemer](http://mitpress.mit.edu/0262562146) with
core.logic make sure to look over
[this](http://github.com/clojure/core.logic/wiki/Differences-from-The-Reasoned-Schemer)
first.

If you're interested in using core.logic from
[ClojureScript](http://github.com/clojure/clojurescript/) look
[here](http://github.com/clojure/core.logic/wiki/Using-core.logic-with-ClojureScript).

For more information & documentation please consult the
[wiki](http://github.com/clojure/core.logic/wiki).

Differences from core.unify
----

[core.unify](http://github.com/clojure/core.unify) provides a la carte
unification facilities that are not deeply tied into the operation of
a logic engine. While core.logic does provide a similar simple unifier
interface with support for specifying fine-grained constraints, if you
have no need for a logic programming system, core.unify may be a
better fit.

YourKit
----

<img src="http://www.yourkit.com/images/yklogo.png"></img>

YourKit has given an open source license for their profiler, greatly
simplifying the profiling of core.logic performance.

YourKit supports open source projects with its full-featured Java
Profiler.  YourKit, LLC is the creator of <a
href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java
Profiler</a> and <a
href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET
Profiler</a>, innovative and intelligent tools for profiling Java and
.NET applications.

Developer information
----

* [Bug Tracker](http://dev.clojure.org/jira/browse/LOGIC)
* [Continuous Integration](http://build.clojure.org/job/core.logic/)
* [Compatibility Test Matrix](http://build.clojure.org/job/core.logic-test-matrix/)

Copyright and license
----

Copyright © 2010-2016 David Nolen, Rich Hickey & contributors.

Licensed under the EPL (see the file epl.html).
