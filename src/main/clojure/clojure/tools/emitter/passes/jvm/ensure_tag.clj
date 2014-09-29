;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.emitter.passes.jvm.ensure-tag
  (:require [clojure.tools.analyzer.passes.jvm.infer-tag :refer [infer-tag]]))

(defn ensure-tag
  {:pass-info {:walk :any :depends #{#'infer-tag}}}
  [{:keys [o-tag tag] :as ast}]
  (assoc ast
    :tag   (or tag Object)
    :o-tag (or o-tag Object)))
