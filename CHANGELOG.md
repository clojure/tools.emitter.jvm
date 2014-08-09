Changelog
========================================
* Release 0.1.0-beta4 on 09 Aug 2014
  * Validation check on unsupported value type to :push
  * Added 1-arity compile-and-load
  * Updated to tools.analyzer.jvm's new AST nodes format
* Release 0.1.0-beta3 on 26 Jul 2014
  * Fixed unused bindings clearing
  * Refactored emit/emit-class
  * Removed the class cache
  * Updated to tools.analyzer.jvm's new AST nodes format
  * Lift loop/try bodies in a separate method
  * Allowed try expressions to return primitive values
  * More consistent class-name munging
* Release 0.1.0-beta2 on 23 Jun 2014
  * Munge deftype fields name
  * Made load cwd aware
  * Performance enhancements
  * Fixed letfn emission
  * Fixed emission of empty maps
  * Added option maps to eval/load
  * Update to tools.analyzer.jvm's new ctx format
* Release 0.1.0-beta1 on 29 Mar 2014
  * Use :internal-name instead of :name to name a fn
  * Fixed class name munging
  * Fixed handling of primitive array types
  * Set macro flag when compiling a Var
  * Bumped class version to 1.6
  * Fixed recur emission
  * Fixed emission of array-maps with more than THRESHOLD elements
* Release 0.1.0-alpha2 on 01 Mar 2014
  * Use clojure.tools.analyzer.jvm's macroexpander
  * Fixed locals emission
* Release 0.1.0-alpha1 on 27 Feb 2014
  * First alpha release
