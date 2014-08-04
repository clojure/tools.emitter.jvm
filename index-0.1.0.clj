{:namespaces
 ({:source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/864db910549f8a4872c3c15b2a12353ce24d2054/src/main/clojure/clojure/tools/emitter/jvm.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm/clojure.tools.emitter.jvm-api.html",
   :name "clojure.tools.emitter.jvm",
   :doc nil}
  {:source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/6484dc80e2e84d75c80ab094e5070cc84e92444c/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm/clojure.tools.emitter.jvm.emit-api.html",
   :name "clojure.tools.emitter.jvm.emit",
   :doc nil}),
 :vars
 ({:arglists
   ([form]
    [form
     {:keys [debug? emit-opts class-loader],
      :or
      {debug? false,
       emit-opts {},
       class-loader (clojure.lang.RT/makeClassLoader)},
      :as options}]),
   :name "eval",
   :namespace "clojure.tools.emitter.jvm",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/864db910549f8a4872c3c15b2a12353ce24d2054/src/main/clojure/clojure/tools/emitter/jvm.clj#L28",
   :raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/864db910549f8a4872c3c15b2a12353ce24d2054/src/main/clojure/clojure/tools/emitter/jvm.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm/eval",
   :doc
   "(eval form)\n (eval form eval-options-map)\n\nForm is a read Clojure s expression represented as a list.\nEval-options-map is a map, defaulting to the empty map, the\nfollowing values of which are significant. Returns the result of\nevaling the input expression.\n\nOptions\n-----------\n:debug? :- (Option Bool)\n  Enables or disables printing in eval. Used as the default value for\n  printing in the emitter.\n\n:emit-opts :- (Option emit-options-map)\n  An options map which will be merged with the default options\n  provided to emit. Keys in this map take precidence over the default\n  values provided to emit. The keys which are significant in this map\n  are documented in the t.e.jvm.emit/emit docstring.",
   :var-type "function",
   :line 28,
   :file "src/main/clojure/clojure/tools/emitter/jvm.clj"}
  {:arglists
   ([res]
    [res
     {:keys [debug? eval-opts class-loader],
      :or
      {debug? false,
       eval-opts {},
       class-loader (clojure.lang.RT/makeClassLoader)},
      :as options}]),
   :name "load",
   :namespace "clojure.tools.emitter.jvm",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/864db910549f8a4872c3c15b2a12353ce24d2054/src/main/clojure/clojure/tools/emitter/jvm.clj#L75",
   :raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/864db910549f8a4872c3c15b2a12353ce24d2054/src/main/clojure/clojure/tools/emitter/jvm.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm/load",
   :doc
   "(load resource)\n (load resource load-options-map)\n\nResource is a string identifier for a Clojure resource on the\nclasspath. Load-options is a a map, defalting to the empty map, in\nwhich the following keys are meaningful. Returns nil.\n\nOptions\n-----------\n:debug? :- (Option Bool)\n  Enables or disables printing in eval. Used as the default value for\n  printing in the emitter.\n\n:eval-opts  :- (Option eval-options-map)\n  An options map which will be merged with the default options\n  provided to eval. Keys set in this map take precidence over the\n  default values supplied to eval. The keys which are significant in\n  this map are documented in the t.e.jvm/eval docstring.\n\n:class-loader :- (Option ClassLoader)\n  An optional classloader into which compiled functions will be\n  injected. If not provided, a new Clojure classloader will be\n  used. If a class loader is provided here, one need not be provided\n  in eval-opts.",
   :var-type "function",
   :line 75,
   :file "src/main/clojure/clojure/tools/emitter/jvm.clj"}
  {:arglists
   ([ast] [{:keys [env o-tag tag op type unchecked?], :as ast} frame]),
   :name "emit",
   :namespace "clojure.tools.emitter.jvm.emit",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/6484dc80e2e84d75c80ab094e5070cc84e92444c/src/main/clojure/clojure/tools/emitter/jvm/emit.clj#L61",
   :raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/6484dc80e2e84d75c80ab094e5070cc84e92444c/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm.emit/emit",
   :doc
   "(λ AST) → Bytecode\n(λ AST → Options) → Bytecode\n\nAST is an analyzed, macroexpanded t.a.jvm AST. Options is a map, the\nfollowing values of which are significant. Returns a (potentially\nempty) sequence of bytecodes. *classes* must be bound before calling\nemit.\n\nOptions\n-----------\n:debug? :- (Option bool)\n  Controls development debug level printing throughout code generation.",
   :var-type "function",
   :line 61,
   :file "src/main/clojure/clojure/tools/emitter/jvm/emit.clj"}
  {:arglists ([ast] [ast opts]),
   :name "emit-classes",
   :namespace "clojure.tools.emitter.jvm.emit",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/6484dc80e2e84d75c80ab094e5070cc84e92444c/src/main/clojure/clojure/tools/emitter/jvm/emit.clj#L99",
   :raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/6484dc80e2e84d75c80ab094e5070cc84e92444c/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm.emit/emit-classes",
   :doc
   "(λ AST) → (Seq Class-AST)\n(λ AST → Options) → (Seq Class-AST)\n\nCompiles the given AST into potentially several classes, returning a\nsequence of ASTs defining classes.\n\nOptions\n-----------\n:debug :- (Option bool)\n  Controls developlent debug level printing throughout code generation.",
   :var-type "function",
   :line 99,
   :file "src/main/clojure/clojure/tools/emitter/jvm/emit.clj"})}
