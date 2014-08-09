(ns clojure.tools.emitter.jvm.core-test
  (:require [clojure.tools.emitter.jvm :as e]
            [clojure.test :refer :all]))

(deftest eval-test
  (is (= 1 (e/eval 1)))
  (is (= :a (e/eval :a)))
  (is (= {:foo [#{"bar"}]} (e/eval {:foo [#{"bar"}]})))
  (is (= 1 (e/eval '((fn [] 1)))))
  (is (= :foo (e/eval '((fn [x] x) :foo))))
  (is (= (range 10) (e/eval '(for [x (range 10)] x))))
  (is (= [1 2] (e/eval '(:foo {:foo [1 2]}))))
  (is (= 3 (e/eval '(first (remove #(not= 3 %) (filter odd? (map inc (range 10)))))))))
