;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.emitter.jvm
  (:refer-clojure :exclude [eval macroexpand-1 load])
  (:require [clojure.tools.analyzer.jvm :as a]
            [clojure.tools.analyzer :refer [macroexpand-1]]
            [clojure.tools.emitter.jvm.emit :as e]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as readers])
  (:import clojure.lang.IFn))

(defn eval
  ([form] (eval form false))
  ([form debug?]
     (let [mform (binding [macroexpand-1 a/macroexpand-1]
                   (macroexpand form))]
       (if (and (seq? mform) (= 'do (first mform)))
         (let [[statements ret] (loop [statements [] [e & exprs] (rest mform)]
                                  (if (seq exprs)
                                    (recur (conj statements e) exprs)
                                    [statements e]))]
           (doseq [expr statements]
             (eval expr debug?))
           (eval ret debug?))
         (let [r (-> (a/analyze `(^:once ^:top-level fn* [] ~mform) (a/empty-env))
                   (e/emit {:debug? debug?
                            :class-loader (clojure.lang.RT/makeClassLoader)}))
               {:keys [class]} (meta r)]
           (.invoke ^IFn (.newInstance ^Class class)))))))

(defn load
  ([res] (load res false))
  ([res debug?]
     (let [p    (str (s/replace (munge res) #"\." "/") ".clj")
           eof  (reify)
           p (if (.startsWith p "/") (subs p 1) p)
           file (-> p io/resource io/reader slurp)
           reader (readers/indexing-push-back-reader file)]
       (with-redefs [clojure.core/load load]
         (binding [*ns* *ns*]
           (loop []
             (let [form (r/read reader false eof)]
               (when (not= eof form)
                 (eval form debug?)
                 (recur)))))))))
