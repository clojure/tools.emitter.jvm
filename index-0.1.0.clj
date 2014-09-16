{:namespaces
 ({:source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/d425aaee221402599cbb75498a619b016dbdb752/src/main/clojure/clojure/tools/emitter/jvm.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm/clojure.tools.emitter.jvm-api.html",
   :name "clojure.tools.emitter.jvm",
   :doc nil}
  {:source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/40b078de543e703610d3c214388fd77910a3780d/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm/clojure.tools.emitter.jvm.emit-api.html",
   :name "clojure.tools.emitter.jvm.emit",
   :doc nil}),
 :vars
 ({:arglists
   ([ast] [{:keys [env o-tag tag op type unchecked?], :as ast} frame]),
   :name "emit",
   :namespace "clojure.tools.emitter.jvm.emit",
   :source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/40b078de543e703610d3c214388fd77910a3780d/src/main/clojure/clojure/tools/emitter/jvm/emit.clj#L61",
   :raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/40b078de543e703610d3c214388fd77910a3780d/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
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
   "https://github.com/clojure/tools.emitter.jvm/blob/40b078de543e703610d3c214388fd77910a3780d/src/main/clojure/clojure/tools/emitter/jvm/emit.clj#L99",
   :raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/40b078de543e703610d3c214388fd77910a3780d/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm.emit/emit-classes",
   :doc
   "(λ AST) → (Seq Class-AST)\n(λ AST → Options) → (Seq Class-AST)\n\nCompiles the given AST into potentially several classes, returning a\nsequence of ASTs defining classes.\n\nOptions\n-----------\n:debug :- (Option bool)\n  Controls developlent debug level printing throughout code generation.",
   :var-type "function",
   :line 99,
   :file "src/main/clojure/clojure/tools/emitter/jvm/emit.clj"})}
