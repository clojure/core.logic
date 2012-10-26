core.logic
====

A Logic Programming library for Clojure & ClojureScript. core.logic offers Prolog-like relational programming and constraint logic programming for Clojure. At its heart is an original implementation of miniKanren as described in William Byrd's dissertation [Relational Programming in miniKanren: Techniques, Applications, and Implementations](http://pqdtopen.proquest.com/#abstract?dispub=3380156) as well as the extensions described in [cKanren](http://www.schemeworkshop.org/2011/papers/Alvis2011.pdf).

If you wish to work through [The Reasoned Schemer](http://mitpress.mit.edu/0262562146) with core.logic make sure to look over [this](http://github.com/clojure/core.logic/wiki/Differences-from-The-Reasoned-Schemer) first.

If you're interested in using core.logic from [ClojureScript](http://github.com/clojure/clojurescript/) look [here](http://github.com/clojure/core.logic/wiki/Using-core.logic-with-ClojureScript).

For more information & documentation please consult the [wiki](http://github.com/clojure/core.logic/wiki).

YourKit
----

YourKit has given an open source license for their profiler, greatly simplifying the profiling of core.logic performance.

YourKit is kindly supporting open source projects with its full-featured Java Profiler. YourKit, LLC is the creator of innovative and intelligent tools for profiling Java and .NET applications. Take a look at YourKit's leading software products:

* <a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
* <a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.

Releases and dependency information
----

Latest beta: 0.8.0-beta2

Latest stable release: 0.7.5

* [All released versions](http://search.maven.org/#search%7Cgav%7C1%7Cg%3A%22org.clojure%22%20AND%20a%3A%22core.logic%22)
* [Development snapshot version](http://oss.sonatype.org/index.html#nexus-search;gav~org.clojure~core.logic~~~)

[Leiningen](http://github.com/technomancy/leiningen/) dependency information:

```
[org.clojure/core.logic "0.7.5"]
```

[Maven](http://maven.apache.org) dependency information:

```
<dependency>
  <groupId>org.clojure</groupId>
  <artifactId>core.logic</artifactId>
  <version>0.7.5</version>
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

Developer information
----

* [Bug Tracker](http://dev.clojure.org/jira/browse/LOGIC)
* [Continuous Integration](http://build.clojure.org/job/core.logic/)
* [Compatibility Test Matrix](http://build.clojure.org/job/core.logic-test-matrix/)

Copyright and license
----

Copyright © 2010-2012 David Nolen, Rich Hickey & contributors.

Licensed under the EPL (see the file epl.html).
