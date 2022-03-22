(ns defstackfn.util-test
  (:require [clojure.test :refer :all]
            [defstackfn.util :refer :all]))

(deftest mk-thread-local-test
  (testing "should get default value"
    (is (false? ((mk-thread-local false)))))

  (testing "should get updated value"
    (is (true? ((mk-thread-local false) :set true)))))

(deftest scan-test
  (testing "nil case"
    (is (= [] (scan nil))))

  (testing "empty case"
    (is (= [] (scan '()))))

  (testing "single case"
    (is (= [[1 []]] (scan '(1)))))

  (testing "multi case"
    (is (= [[1 []]
            [2 [1]]
            [3 [1 2]]]
           (scan '(1 2 3))))))
