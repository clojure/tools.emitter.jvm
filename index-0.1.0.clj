{:namespaces
 ({:doc nil,
   :name "clojure.tools.emitter.jvm",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm/clojure.tools.emitter.jvm-api.html",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/4bd3bcceb08e43334381db77041e527615df70ee/src/main/clojure/clojure/tools/emitter/jvm.clj"}
  {:doc nil,
   :name "clojure.tools.emitter.jvm.emit",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm/clojure.tools.emitter.jvm.emit-api.html",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/08767430b14273183666526006049a6d09e79418/src/main/clojure/clojure/tools/emitter/jvm/emit.clj"}),
 :vars
 ({:raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/4bd3bcceb08e43334381db77041e527615df70ee/src/main/clojure/clojure/tools/emitter/jvm.clj",
   :name "eval",
   :file "src/main/clojure/clojure/tools/emitter/jvm.clj",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/4bd3bcceb08e43334381db77041e527615df70ee/src/main/clojure/clojure/tools/emitter/jvm.clj#L74",
   :line 74,
   :var-type "function",
   :arglists
   ([form]
    [form
     {:keys [debug? emit-opts class-loader analyze-opts compile-files],
      :or
      {debug? false,
       emit-opts {},
       analyze-opts default-passes-opts,
       compile-files
       (if (bound? #'*compile-files*) *compile-files* false),
       class-loader (makeClassLoader)},
      :as options}]),
   :doc
   "(eval form)\n (eval form eval-options-map)\n\nForm is a read Clojure s expression represented as a list.\nEval-options-map is a map, defaulting to the empty map, the\nfollowing values of which are significant. Returns the result of\nevaling the input expression.\n\nOptions\n-----------\n:debug? :- (Option Bool)\n  Enables or disables printing in eval. Used as the default value for\n  printing in the emitter.\n\n:emit-opts :- (Option emit-options-map)\n  An options map which will be merged with the default options\n  provided to emit. Keys in this map take precidence over the default\n  values provided to emit. The keys which are significant in this map\n  are documented in the t.e.jvm.emit/emit docstring.\n\n:analyze-opts :- (Option analyze-options-map)\n  An options map that will be passed to the analyzer. The keys which\n  are significant in this map are documented in the t.a.jvm/analyze\n  docstring.\n\n:class-loader :- (Option ClassLoader)\n  An optional classloader into which compiled functions will be\n  injected. If not provided, a new Clojure classloader will be\n  used. If a class loader is provided here, one need not be provided\n  in eval-opts.\n\n:compile-files :- (Option Bool)\n  Enables or disables writing classfiles for generated classes. False\n  by default.",
   :namespace "clojure.tools.emitter.jvm",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm/eval"}
  {:raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/4bd3bcceb08e43334381db77041e527615df70ee/src/main/clojure/clojure/tools/emitter/jvm.clj",
   :name "load",
   :file "src/main/clojure/clojure/tools/emitter/jvm.clj",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/4bd3bcceb08e43334381db77041e527615df70ee/src/main/clojure/clojure/tools/emitter/jvm.clj#L140",
   :line 140,
   :var-type "function",
   :arglists
   ([res]
    [res
     {:keys [debug? eval-opts class-loader compile-files],
      :or
      {debug? false,
       eval-opts {},
       compile-files
       (if (bound? #'*compile-files*) *compile-files* false),
       class-loader (makeClassLoader)},
      :as options}]),
   :doc
   "(load resource)\n (load resource load-options-map)\n\nResource is a string identifier for a Clojure resource on the\nclasspath. Load-options is a a map, defalting to the empty map, in\nwhich the following keys are meaningful. Returns nil.\n\nOptions\n-----------\n:debug? :- (Option Bool)\n  Enables or disables printing in eval. Used as the default value for\n  printing in the emitter.\n\n:eval-opts  :- (Option eval-options-map)\n  An options map which will be merged with the default options\n  provided to eval. Keys set in this map take precidence over the\n  default values supplied to eval. The keys which are significant in\n  this map are documented in the t.e.jvm/eval docstring.\n\n:class-loader :- (Option ClassLoader)\n  An optional classloader into which compiled functions will be\n  injected. If not provided, a new Clojure classloader will be\n  used. If a class loader is provided here, one need not be provided\n  in eval-opts.\n\n:compile-files :- (Option Bool)\n  Enables or disables writing classfiles for generated classes. False\n  by default.",
   :namespace "clojure.tools.emitter.jvm",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm/load"}
  {:raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/4bd3bcceb08e43334381db77041e527615df70ee/src/main/clojure/clojure/tools/emitter/jvm.clj",
   :name "write-class",
   :file "src/main/clojure/clojure/tools/emitter/jvm.clj",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/4bd3bcceb08e43334381db77041e527615df70ee/src/main/clojure/clojure/tools/emitter/jvm.clj#L34",
   :line 34,
   :var-type "function",
   :arglists ([name bytecode]),
   :doc
   "(λ ClassName → Bytecode) → Nil\n\nWrites the given bytecode to a file named by the ClassName and\n*compile-path*. Requires that *compile-path* be set. Returns Nil.",
   :namespace "clojure.tools.emitter.jvm",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm/write-class"}
  {:raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/08767430b14273183666526006049a6d09e79418/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
   :name "emit",
   :file "src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/08767430b14273183666526006049a6d09e79418/src/main/clojure/clojure/tools/emitter/jvm/emit.clj#L61",
   :line 61,
   :var-type "function",
   :arglists
   ([ast] [{:keys [env o-tag tag op type unchecked?], :as ast} frame]),
   :doc
   "(λ AST) → Bytecode\n(λ AST → Options) → Bytecode\n\nAST is an analyzed, macroexpanded t.a.jvm AST. Options is a map, the\nfollowing values of which are significant. Returns a (potentially\nempty) sequence of bytecodes. *classes* must be bound before calling\nemit.\n\nOptions\n-----------\n:debug? :- (Option bool)\n  Controls development debug level printing throughout code generation.",
   :namespace "clojure.tools.emitter.jvm.emit",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm.emit/emit"}
  {:raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/08767430b14273183666526006049a6d09e79418/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
   :name "emit-classes",
   :file "src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/08767430b14273183666526006049a6d09e79418/src/main/clojure/clojure/tools/emitter/jvm/emit.clj#L99",
   :line 99,
   :var-type "function",
   :arglists ([ast] [ast opts]),
   :doc
   "(λ AST) → (Seq Class-AST)\n(λ AST → Options) → (Seq Class-AST)\n\nCompiles the given AST into potentially several classes, returning a\nsequence of ASTs defining classes.\n\nOptions\n-----------\n:debug :- (Option bool)\n  Controls developlent debug level printing throughout code generation.",
   :namespace "clojure.tools.emitter.jvm.emit",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm.emit/emit-classes"})}
