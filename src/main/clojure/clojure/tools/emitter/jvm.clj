;:   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.emitter.jvm
  (:refer-clojure :exclude [eval])
  (:require [clojure.tools.analyzer.jvm :as a]
            [clojure.tools.emitter.jvm.emit :as e])
  (:import clojure.lang.IFn))

(defn eval
  ([form] (eval form false))
  ([form debug?]
     (let [r (e/emit (a/analyze `(^:once fn* [] ~form) (a/empty-env))
                     {:debug? debug?
                      :class-loader (clojure.lang.RT/makeClassLoader)})
           {:keys [class]} (meta r)]
       (.invoke ^IFn (.newInstance ^Class class)))))
