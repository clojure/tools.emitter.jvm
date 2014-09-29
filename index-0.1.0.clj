{:namespaces
 ({:source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/9dd866390082138e27973e030d73cb7ff7420057/src/main/clojure/clojure/tools/emitter/jvm.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm/clojure.tools.emitter.jvm-api.html",
   :name "clojure.tools.emitter.jvm",
   :doc nil}
  {:source-url
   "https://github.com/clojure/tools.emitter.jvm/blob/08767430b14273183666526006049a6d09e79418/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
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
   "https://github.com/clojure/tools.emitter.jvm/blob/08767430b14273183666526006049a6d09e79418/src/main/clojure/clojure/tools/emitter/jvm/emit.clj#L61",
   :raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/08767430b14273183666526006049a6d09e79418/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
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
   "https://github.com/clojure/tools.emitter.jvm/blob/08767430b14273183666526006049a6d09e79418/src/main/clojure/clojure/tools/emitter/jvm/emit.clj#L99",
   :raw-source-url
   "https://github.com/clojure/tools.emitter.jvm/raw/08767430b14273183666526006049a6d09e79418/src/main/clojure/clojure/tools/emitter/jvm/emit.clj",
   :wiki-url
   "http://clojure.github.com/tools.emitter.jvm//clojure.tools.emitter.jvm-api.html#clojure.tools.emitter.jvm.emit/emit-classes",
   :doc
   "(λ AST) → (Seq Class-AST)\n(λ AST → Options) → (Seq Class-AST)\n\nCompiles the given AST into potentially several classes, returning a\nsequence of ASTs defining classes.\n\nOptions\n-----------\n:debug :- (Option bool)\n  Controls developlent debug level printing throughout code generation.",
   :var-type "function",
   :line 99,
   :file "src/main/clojure/clojure/tools/emitter/jvm/emit.clj"})}
