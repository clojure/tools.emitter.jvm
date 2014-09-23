;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.emitter.passes.jvm.collect-internal-methods
  (:require [clojure.tools.analyzer.ast :refer [update-children]]))

(def ^:dynamic ^:private *internal-methods*)

(defn collect-internal-methods
  {:pass-info {:walk :none :depends #{} :compiler true}} ;; ensure it's run last
  [ast]
  (case (:op ast)
    (:method :fn-method)
    (binding [*internal-methods* (atom [])]
      (let [ast (update-children ast collect-internal-methods)]
        (merge ast
               (when-let [m (seq @*internal-methods*)]
                 {:internal-methods m}))))

    (:try :loop)
    (let [ast (update-children (assoc ast :internal-method-name
                                      (or (:loop-id ast) (gensym "try__")))
                               collect-internal-methods)]
      (swap! *internal-methods* conj ast)
      ast)

    (update-children ast collect-internal-methods)))
