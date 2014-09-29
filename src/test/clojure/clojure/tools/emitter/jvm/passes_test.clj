(ns clojure.tools.emitter.jvm.passes-test
  (:require [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.tools.analyzer.passes :refer [schedule]]
            [clojure.test :refer [deftest is]]
            [clojure.set :as set]
            [clojure.tools.analyzer.passes.collect-closed-overs :refer [collect-closed-overs]]
            [clojure.tools.emitter.passes.jvm.clear-locals :refer [clear-locals]]
            [clojure.tools.emitter.passes.jvm.collect :refer [collect]])
  (:import (clojure.lang Keyword Var PersistentArrayMap)))

(defmacro ast1 [form]
  `(ana.jvm/analyze '~form))

(deftest collect-test
  (binding [ana.jvm/run-passes (schedule (conj ana.jvm/default-passes #'collect #'collect-closed-overs))]
    (let [c-test (-> (ast1 (let [a 1 b 2] (fn [x] (fn [] [+ (:foo {}) x a]))))
                   :body :ret)]
      (is (= '#{a__#0} (-> c-test :closed-overs keys set)))
      (is (set/subset? #{{:form :foo
                          :tag  Keyword
                          :meta nil}
                         {:form #'+
                          :meta (meta #'+)
                          :tag  Var}
                         {:form {}
                          :tag  PersistentArrayMap
                          :meta nil}}
                       (-> c-test :methods first :body :ret :constants keys set))) ;; it registers metadata too (line+col info)
      (is (= '#{a__#0 x__#0} (-> c-test :methods first :body :ret :closed-overs keys set))))))

(deftest clear-locals-test
  (binding [ana.jvm/run-passes (schedule (conj ana.jvm/default-passes #'clear-locals))]
    (let [f-expr (-> (ast1 (fn [x] (if x x x) x (if x (do x x) (if x x x))))
                   :methods first :body)]
      (is (= true (-> f-expr :statements first :then :to-clear? nil?)))
      (is (= true (-> f-expr :statements first :else :to-clear? nil?)))
      (is (= true (-> f-expr :statements second :to-clear? nil?)))
      (is (= true (-> f-expr :ret :then :statements first :to-clear? nil?)))
      (is (= true (-> f-expr :ret :then :ret :to-clear?)))
      (is (= true (-> f-expr :ret :else :then :to-clear?)))
      (is (= true (-> f-expr :ret :else :else :to-clear?))))
    (let [f-expr (-> (ast1 (fn [x] (loop [a x] (if 1 x (do x (recur x))))))
                   :methods first :body :ret)]
      (is (= true (-> f-expr :bindings first :init :to-clear? nil?)))
      (is (= true (-> f-expr :body :ret :then :to-clear?)))
      (is (= true (-> f-expr :body :ret :else :statements first :to-clear? nil?)))
      (is (= true (-> f-expr :body :ret :else :ret :exprs first :to-clear? nil?))))
    (let [f-expr (-> (ast1 (loop [] (let [a 1] (loop [] a)) (recur)))
                   :body :statements first :body :ret :body :ret)]
      (is (= true (-> f-expr :to-clear?))))
    (let [f-expr (-> (ast1 (loop [] (let [a 1] (loop [] (if 1 a (recur)))) (recur)))
                   :body :statements first :body :ret :body :ret :then)]
      (is (= true (-> f-expr :to-clear?))))
    (let [f-expr (-> (ast1 (let [a 1] (loop [] (let [b 2] (loop [] (if 1 [a b] (recur)))) (recur))))
                   :body :ret :body :statements first :body :ret :body :ret :then :items)]
      (is (= true (-> f-expr first :to-clear? nil?)))
      (is (= true (-> f-expr second :to-clear?))))
    (let [f-expr (-> (ast1 (let [a 1] (loop [] (if 1 a) (recur))))
                   :body :ret :body :statements first :then)]
      (is (= true (-> f-expr :to-clear? nil?))))
    (let [f-expr (-> (ast1 (let [a 1] (loop [] (let [x (if 1 a)]) (recur))))
                   :body :ret :body :statements first :bindings first :init :then)]
      (is (= true (-> f-expr :to-clear? nil?))))))
