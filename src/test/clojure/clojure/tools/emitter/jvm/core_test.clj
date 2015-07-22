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
                    @x))))
  (is (= 12
         (e/eval '(letfn [(six-times [y]
                            (* (twice y) 3))
                          (twice [x]
                            (* x 2))]
                    (six-times 2)))))
  (let [f (e/eval
           '(fn [x]
              (case x
                (1) 1
                (:a) 2
                ([:x 1] :y) 3
                :default)))]
    (is (= 1 (f 1)))
    (is (= 2 (f :a)))
    (is (= :default (f 6)))
    (is (= 3 (f :y)))
    (is (= 3 (f [:x 1])))))



;; (deftest load-core-test
;;   (is (= nil (e/load "/clojure.core"))))
