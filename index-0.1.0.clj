{:namespaces
 ({:source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/61a7375717c6eb64a858d6b7e56c36ed9a246035/src/main/clojure/clojure/tools/emitter/jvm.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm/clojure.tools.emitter.jvm-api.html",
   :name "clojure.tools.emitter.jvm",
   :doc nil}
  {:source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/6221fdd8982a07dfb9991c07a28215fb379655bb/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm/clojure.tools.emitter.jvm.emit-api.html",
   :name "clojure.tools.emitter.jvm.emit",
   :doc nil}),
 :vars
 ({:arglists
   ([form]
    [form
     {:keys [debug? emit-opts],
      :or {debug? false, emit-opts {}},
      :as options}]),
   :name "eval",
   :namespace "clojure.tools.emitter.jvm",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/61a7375717c6eb64a858d6b7e56c36ed9a246035/src/main/clojure/clojure/tools/emitter/jvm.clj#L20",
   :raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/61a7375717c6eb64a858d6b7e56c36ed9a246035/src/main/clojure/clojure/tools/emitter/jvm.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm/eval",
   :doc
   "(eval form)\n (eval form eval-options-map)\n\nForm is a read Clojure s expression represented as a list.\nEval-options-map is a map, defaulting to the empty map, the\nfollowing values of which are significant. Returns the result of\nevaling the input expression.\n\nOptions\n-----------\n:debug? :- (Option Bool)\n  Enables or disables printing in eval. Used as the default value for\n  printing in the emitter.\n\n:emit-opts :- (Option emit-options-map)\n  An options map which will be merged with the default options\n  provided to emit. Keys in this map take precidence over the default\n  values provided to emit. The keys which are significant in this map\n  are documented in the t.e.jvm.emit/emit docstring.",
   :var-type "function",
   :line 20,
   :file "src/main/clojure/clojure/tools/emitter/jvm.clj"}
  {:arglists
   ([ast] [{:keys [env o-tag tag op type unchecked?], :as ast} frame]),
   :name "emit",
   :namespace "clojure.tools.emitter.jvm.emit",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/6221fdd8982a07dfb9991c07a28215fb379655bb/src/main/clojure/clojure/tools/emitter/jvm/emit.clj#L60",
   :raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/6221fdd8982a07dfb9991c07a28215fb379655bb/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm.emit/emit",
   :doc
   "(emit ast)\n (emit ast emit-options-map)\n\nAST is an analyzed, macroexpanded t.a.jvm AST. emit-options-map is a\nmap, the following values of which are significant. Returns a\nbytecode descriptor vector for the last compile class annotated with\nclass metadata. All compiled classes are loaded into the\nclass-loader (defaulting to the standard Clojure classloader) for\nside-effects.\n\nOptions\n-----------\n:debug? :- (Option bool)\n  Controls development debug level printing throughout code generation.\n\n:class-loader :- (Option classloader)\n  A classloader instance which will be used for loading the resulting\n  bytecode into the host JVM.",
   :var-type "function",
   :line 60,
   :file "src/main/clojure/clojure/tools/emitter/jvm/emit.clj"})}
