;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.emitter.jvm
  (:refer-clojure :exclude [eval macroexpand-1 macroexpand load])
  (:require [clojure.tools.analyzer.jvm :as a]
            [clojure.tools.analyzer :refer [macroexpand-1 macroexpand]]
            [clojure.tools.analyzer.passes :refer [schedule]]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.utils :refer [mmerge]]
            [clojure.tools.emitter.jvm.emit :as e]
            [clojure.tools.emitter.jvm.transform :as t]
            [clojure.tools.analyzer.passes
             [collect-closed-overs :refer [collect-closed-overs]]
             [trim :refer [trim]]]
            [clojure.tools.emitter.passes.jvm
             [collect :refer [collect]]
             [collect-internal-methods :refer :all]
             [clear-locals :refer [clear-locals]]
             [annotate-class-id :refer [annotate-class-id]]
             [annotate-internal-name :refer [annotate-internal-name]]
             [ensure-tag :refer [ensure-tag]]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as readers])
  (:import (clojure.lang IFn DynamicClassLoader Atom)))

(defn write-class
  "(λ ClassName → Bytecode) → Nil

  Writes the given bytecode to a file named by the ClassName and
  *compile-path*. Requires that *compile-path* be set. Returns Nil."
  [name bytecode]
  {:pre [(bound? #'clojure.core/*compile-path*)]}
  (let [path (str *compile-path* "/" name ".class")
        file (io/file path)]
    (.mkdirs (io/file (.getParent file)))
    (with-open [w (java.io.FileOutputStream. path)]
      (.write w bytecode)))
  nil)

(defn compile-and-load
  ([class-ast]
   (compile-and-load class-ast (clojure.lang.RT/makeClassLoader)))
  ([{:keys [class-name] :as class-ast} class-loader]
   (let [bytecode (t/-compile class-ast)]
     (when (and (bound? #'clojure.core/*compile-files*)
                *compile-files*)
       (write-class class-name bytecode))
     (.defineClass ^DynamicClassLoader class-loader class-name bytecode nil))))


(def passes (into (disj a/default-passes #'trim)
                  #{#'collect-internal-methods

                    #'ensure-tag

                    #'annotate-class-id
                    #'annotate-internal-name

                    #'collect
                    #'collect-closed-overs
                    #'clear-locals}))

(def run-passes
  (schedule passes))

(defn eval
  "(eval form)
   (eval form eval-options-map)

  Form is a read Clojure s expression represented as a list.
  Eval-options-map is a map, defaulting to the empty map, the
  following values of which are significant. Returns the result of
  evaling the input expression.

  Options
  -----------
  :debug? :- (Option Bool)
    Enables or disables printing in eval. Used as the default value for
    printing in the emitter.

  :emit-opts :- (Option emit-options-map)
    An options map which will be merged with the default options
    provided to emit. Keys in this map take precidence over the default
    values provided to emit. The keys which are significant in this map
    are documented in the t.e.jvm.emit/emit docstring.

  :analyze-opts :- (Option analyze-options-map)
    An options map that will be passed to the analyzer. The keys which
    are significant in this map are documented in the t.a.jvm/analyze
    docstring.

  :class-loader :- (Option ClassLoader)
    An optional classloader into which compiled functions will be
    injected. If not provided, a new Clojure classloader will be
    used. If a class loader is provided here, one need not be provided
    in eval-opts.

  :compile-files :- (Option Bool)
    Enables or disables writing classfiles for generated classes. False
    by default."

  ([form]
   (eval form {}))
  ([form {:keys [debug? emit-opts class-loader analyze-opts compile-files]
          :or {debug?        false
               emit-opts     {}
               analyze-opts  a/default-passes-opts
               compile-files false
               class-loader  (clojure.lang.RT/makeClassLoader)}
          :as options}]
   {:pre [(instance? DynamicClassLoader class-loader)]}
   (let [mform (binding [macroexpand-1 a/macroexpand-1]
                 (macroexpand form (a/empty-env)))]
     (if (and (seq? mform) (= 'do (first mform)))
       (let [[statements ret] (loop [statements [] [e & exprs] (rest mform)]
                                (if (seq exprs)
                                  (recur (conj statements e) exprs)
                                  [statements e]))]
         (doseq [expr statements]
           (eval expr options))
         (eval ret options))
       (binding [a/run-passes    run-passes
                 *compile-files* compile-files]
         (let [cs (-> (a/analyze `(^:once fn* [] ~mform) (a/empty-env) analyze-opts)
                      (e/emit-classes (merge {:debug? debug?} emit-opts)))
               classes (mapv #(compile-and-load % class-loader) cs)]
           ((.newInstance ^Class (last classes)))))))))

(def root-directory @#'clojure.core/root-directory)

(defn load
  "(load resource)
   (load resource load-options-map)

  Resource is a string identifier for a Clojure resource on the
  classpath. Load-options is a a map, defalting to the empty map, in
  which the following keys are meaningful. Returns nil.

  Options
  -----------
  :debug? :- (Option Bool)
    Enables or disables printing in eval. Used as the default value for
    printing in the emitter.

  :eval-opts  :- (Option eval-options-map)
    An options map which will be merged with the default options
    provided to eval. Keys set in this map take precidence over the
    default values supplied to eval. The keys which are significant in
    this map are documented in the t.e.jvm/eval docstring.

  :class-loader :- (Option ClassLoader)
    An optional classloader into which compiled functions will be
    injected. If not provided, a new Clojure classloader will be
    used. If a class loader is provided here, one need not be provided
    in eval-opts.

  :compile-files :- (Option Bool)
    Enables or disables writing classfiles for generated classes. False
    by default."

  ([res]
   (load res {}))
  ([res {:keys [debug? eval-opts class-loader compile-files]
         :or   {debug?        false
                eval-opts     {}
                compile-files false
                class-loader  (clojure.lang.RT/makeClassLoader)}
         :as options}]
   (let [p      (str (apply str (replace {\. \/ \- \_} res)) ".clj")
         eof    (Object.)
         p      (if (.startsWith p "/")
                  (subs p 1)
                  (subs (str (root-directory (ns-name *ns*)) "/" p) 1))
         file   (-> p io/resource io/reader slurp)
         reader (readers/indexing-push-back-reader file 1 p)]
     (binding [*ns*            *ns*
               *file*          p
               *compile-files* compile-files]
       (loop []
         (let [form (r/read reader false eof)]
           (when (not= eof form)
             (eval form (merge 
                         (when class-loader
                           {:class-loader  class-loader
                            :compile-files compile-files})))
             (recur))))
       nil))))
