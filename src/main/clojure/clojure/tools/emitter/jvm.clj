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
            [clojure.tools.emitter.jvm.emit :as e]
            [clojure.tools.emitter.jvm.transform :as t]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as readers])
  (:import clojure.lang.IFn))

(def ^:dynamic *class-cache*)
(def ^:dynamic *class-loader*)

(defn- compile-and-load
  [{:keys [name class-id] :as class-ast}]
  {:pre [(bound? *class-cache*)
         (instance? clojure.lang.Atom *class-cache*)
         (bound? *class-loader*)
         (instance? java.lang.ClassLoader *class-loader*)]}
  (or (@*class-cache* class-id)
      (let [class (.defineClass *class-loader* name (t/-compile class-ast) nil)]
        (swap! *class-cache* assoc class-id class)
        class)))

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

  Warning
  -----------
    Eval requires that the *class-loader* and *class-cache* atoms be
    bound and will fail if they are not set."

  ([form]
     (eval form {}))

  ([form {:keys [debug? emit-opts]
          :or {debug?    false
               emit-opts {}}
          :as options}]
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
         (let [r       (-> (a/analyze `(^:once fn* [] ~mform) (a/empty-env))
                           (e/emit-classes (merge {:debug? debug?} emit-opts)))
               ;; FIXME:
               ;;   Using mapv for side-effects is kinda
               ;;   jank. Arguably something that should be fixed, but
               ;;   it works.
               classes (mapv compile-and-load r)
               class   (last classes)]
           (.invoke ^IFn (.newInstance ^Class class)))))))

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
    in eval-opts."

  ([res]
     (load res {}))

  ([res {:keys [debug? eval-opts class-loader]
         :or   {debug?       false
                eval-opts    {}
                class-loader (clojure.lang.RT/makeClassLoader)}
         :as options}]
     {:pre [(instance? java.lang.ClassLoader class-loader)]}
     (let [p      (str (apply str (replace {\. \/ \- \_} res)) ".clj")
           eof    (Object.)
           p      (if (.startsWith p "/")
                    (subs p 1)
                    (subs (str (root-directory (ns-name *ns*)) "/" p) 1))
           file   (-> p io/resource io/reader slurp)
           reader (readers/indexing-push-back-reader file 1 p)]
       (binding [*ns*           *ns*
                 *file*         p
                 *class-cache*  (atom {})
                 *class-loader* class-loader]
         (loop []
           (let [form (r/read reader false eof)]
             (when (not= eof form)
               (eval form eval-opts)
               (recur))))
         nil))))
