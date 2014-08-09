# tools.emitter.jvm

A JVM bytecode generator for ASTs compatible with tools.analyzer[.jvm].

* [Example Usage](#example-usage)
* [Differences From Clojure](#differences-from-clojure)
* [Releases and Dependency Information](#releases-and-dependency-information)
* [Changelog](#changelog)
* [Developer Information](#developer-information)
* [License](#license)

## Example Usage

```clojure
user=> (require '[clojure.tools.emitter.jvm :as e])
nil
user=> (e/eval '(+ 1 2))
3
user=> (e/eval '(+ 1 2) {:debug? true})
// class version 50.0 (50)
// access flags 0x31
public final class user$fn__6242 extends clojure/lang/AFunction  implements clojure/lang/IFn$L  {

  // compiled from: user$fn__6242

  // access flags 0x9
  public static <clinit>()V
    RETURN
    MAXSTACK = 0
    MAXLOCALS = 0

  // access flags 0x1
  public <init>()V
    ALOAD 0
    INVOKESPECIAL clojure/lang/AFunction.<init> ()V
    RETURN
    MAXSTACK = 1
    MAXLOCALS = 1

  // access flags 0x1
  public invokePrim()J
   L0
    LINENUMBER 1 L0
    LCONST_1
    LDC 2
    ACONST_NULL
    ASTORE 0
    INVOKESTATIC clojure/lang/Numbers.add (JJ)J
    LRETURN
    LOCALVARIABLE this Lclojure/lang/AFunction; L0 L0 0
    MAXSTACK = 5
    MAXLOCALS = 1

  // access flags 0x1
  public invoke()Ljava/lang/Object;
    ALOAD 0
    INVOKEVIRTUAL user$fn__6242.invokePrim ()J
    INVOKESTATIC clojure/lang/RT.box (J)Ljava/lang/Number;
    ARETURN
    MAXSTACK = 2
    MAXLOCALS = 1
}
3
user=> (e/load "clojure.pprint")
nil
user=> (clojure.pprint/pprint 1)
1
```

## Differences From Clojure
While the bytecode produced by `tools.emitter.jvm` is similar to the one produces by Clojure itself, there are some differences:
* `tools.emitter.jvm` is capable of clearing locals closed over by loops, in the loop exit path
* `tools.emitter.jvm` clears the "this" local before the last tail call in a method, see [CLJ-1250](http://dev.clojure.org/jira/browse/CLJ-1250)
* `tools.emitter.jvm` clears unused locals after their creation
* `tools.emitter.jvm` hoists loop and try bodies into separate methods rather than wrapping them in a fn, see [CLJ-701](http://dev.clojure.org/jira/browse/CLJ-701)
* `tools.emitter.jvm` emits keyword invoke callsites only when the keyword is not namespaces, see [CLJ-1469](http://dev.clojure.org/jira/browse/CLJ-1469)
* `tools.emitter.jvm` emits typed bytecode, enforcing any explicit tag, this breaks some functions in `clojure.core` like `ns-interns`. This behaviour might be reconsidered in the future.
* `tools.emitter.jvm` handles [CLJ-1330](http://dev.clojure.org/jira/browse/CLJ-1330)

## SPONSORSHIP

* Cognitect (http://cognitect.com/) is sponsoring tools.emitter.jvm development (https://groups.google.com/d/msg/clojure/iaP16MHpX0E/EMtnGmOz-rgJ)
* Ambrose BS (https://twitter.com/ambrosebs) has sponsored tools.emitter.jvm development in his typed clojure campaign (http://www.indiegogo.com/projects/typed-clojure).

## YourKit

YourKit has given an open source license for their profiler, greatly simplifying the profiling of tools.emitter.jvm performance.

YourKit is kindly supporting open source projects with its full-featured Java Profiler. YourKit, LLC is the creator of innovative and intelligent tools for profiling Java and .NET applications. Take a look at YourKit's leading software products:

* <a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
* <a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.

## Releases and Dependency Information

Latest stable release: 0.1.0-beta5

* [All Released Versions](http://search.maven.org/#search%7Cgav%7C1%7Cg%3A%22org.clojure%22%20AND%20a%3A%22tools.emitter.jvm%22)

* [Development Snapshot Versions](https://oss.sonatype.org/index.html#nexus-search;gav%7Eorg.clojure%7Etools.emitter.jvm%7E%7E%7E)

[Leiningen](https://github.com/technomancy/leiningen) dependency information:

```clojure
[org.clojure/tools.emitter.jvm "0.1.0-beta5"]
```
[Maven](http://maven.apache.org/) dependency information:

```xml
<dependency>
  <groupId>org.clojure</groupId>
  <artifactId>tools.emitter.jvm</artifactId>
  <version>0.1.0-beta5</version>
</dependency>
```

[Changelog](CHANGELOG.md)
========================================

Developer Information
========================================

* [GitHub project](https://github.com/clojure/tools.emitter.jvm)

* [API index](http://clojure.github.io/tools.emitter.jvm)

* [Bug Tracker](http://dev.clojure.org/jira/browse/TEMJVM)

* [Continuous Integration](http://build.clojure.org/job/tools.emitter.jvm/)

* [Compatibility Test Matrix](http://build.clojure.org/job/tools.emitter.jvm-test-matrix/)

## License

Copyright © 2013-2014 Nicola Mometto, Rich Hickey & contributors.

Distributed under the Eclipse Public License, the same as Clojure.
