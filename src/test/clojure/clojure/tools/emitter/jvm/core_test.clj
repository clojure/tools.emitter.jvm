(ns clojure.tools.emitter.jvm.core-test
  (:require [clojure.tools.emitter.jvm :as e]
            [clojure.test :refer :all]
            [clojure.tools.analyzer.passes :refer [schedule]]))

(deftest eval-test
  (is (= 1 (e/eval 1)))
  (is (= :a (e/eval :a)))
  (is (= {:foo [#{"bar"}]} (e/eval {:foo [#{"bar"}]})))
  (is (= 1 (e/eval '((fn [] 1)))))
  (is (= :foo (e/eval '((fn [x] x) :foo))))
  (is (= 1225 (e/eval '(apply + (range 50)))))
  (is (= 1225 (e/eval `(+ ~@(range 50)))))
  (is (= (range 10) (e/eval '(for [x (range 10)] x))))
  (is (= [1 2] (e/eval '(:foo {:foo [1 2]}))))
  (is (= 3 (e/eval '(do (def a 3) 3))))
  (is (= 6 (e/eval '(do (def g (fn [x] (* x 3))) (g 2)))))
  (is (= 3 (e/eval '(first (remove #(not= 3 %) (filter odd? (map inc (range 10))))))))
  (is (thrown? Exception (e/eval '(throw (Exception. "Foo!")))))
  (is (= 'foo (e/eval '(quote foo))))
  (is (= #'conj (e/eval '(var conj))))
  (is (= {:a :b}
         (e/eval '(let [x :b] {:a x}))
         (e/eval '{:a :b})))
  (is (= #{:a :b}
         (e/eval '#{:a :b})))
  (is (= [:a :b]
         (e/eval '[:a :b])))
  (is (= {:a :b}
         (meta (e/eval '(with-meta [:c :d] {:a :b})))))
  (is (= :catch
         (e/eval '(try (throw (Exception. "Foo!"))
                       (catch Exception e :catch)))))
  (is (= :finally
         (e/eval '(let [x (atom 0)]
                    (try (throw (Exception. "Foo!"))
                         (catch Exception e :catch)
                         (finally (reset! x :finally)))
                    @x)))))



;; (deftest load-core-test
;;   (is (= nil (e/load "/clojure.core"))))
