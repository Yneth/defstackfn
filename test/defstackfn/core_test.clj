(ns defstackfn.core-test
  (:require [clojure.test :refer :all]
            [defstackfn.core :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(deftest test-read-all-s-exps
  (testing "base case"
    (is (= '((defstackfn name [] 1))
           (read-all-s-exps "(defstackfn name [] 1)"))))

  (testing "multi object"
    (is (= '((defstackfn name [] 1) (name))
           (read-all-s-exps "(defstackfn name [] 1) (name)")))))

(deftest test-interpret


  (testing "should execute function"
    (is (nil? (interpret '((defstackfn name [])
                           (name))))))

  (testing "should push constants to stack"
    (is (= "test" (interpret '((defstackfn name [] 1 "test")
                               (name))))))

  (testing "should work with variables"
    (is (nil? (interpret '((defstackfn name [!a !b])
                           (name 1 2))))))

  (testing "should push variable to stack"
    (is (= 2 (interpret '((defstackfn name [!a] !a)
                          (name 2))))))

  (testing "should shadow variable"
    (is (= 200 (interpret '((defstackfn name [!a]
                                        100
                                        !a+
                                        !a
                                        (invoke> + 2))
                            (name 2))))))

  (testing "should invoke clojure multiplication"
    (is (= 4 (interpret '((defstackfn name [!a] !a !a (invoke> * 2))
                          (name 2))))))

  (testing "should execute pop"
    (is (= 100 (interpret '((defstackfn name [!a] !a !a <pop> <pop> 100)
                            (name 2))))))

  (testing "should execute if branch"
    (is (= "hi" (interpret '((defstackfn f [] true (if> "hi" else> "bye"))
                             (f))))))

  (testing "should execute else branch"
    (is (= "bye" (interpret '((defstackfn f [] false (if> "hi" else> "bye"))
                              (f))))))

  (testing "should execute nested ifs"
    (is (= "_second_" (interpret '((defstackfn f []
                                               true
                                               (if>
                                                 "hi"
                                                 (if>
                                                   "_"
                                                   "second"
                                                   "_"
                                                   (invoke> str 3))
                                                 else> "bye"))
                                   (f))))))

  (testing "should execute when no else"
    (is (= nil (interpret '((defstackfn f []
                                        false
                                        (if>
                                          "lol"))
                            (f))))))

  (testing "should fail when else without if"
    (is (thrown? ExceptionInfo
                 (interpret '((defstackfn f []
                                          false
                                          else>
                                          "lol")
                              (f))))))

  (testing "should allow nested functions"
    (is (= 101
           (interpret '((defstackfn const1 [] 1)
                        (defstackfn sum [!a !b] !a !b (invoke> + 2))
                        (defstackfn func []
                                    100
                                    (invoke> const1 0)
                                    (invoke> sum 2))
                        (func))))))

  (testing "should disallow anonymous functions"
    (is (thrown? ExceptionInfo
                 (interpret '((defstackfn f []
                                          true
                                          (invoke> #(str "_" % "_") 1))
                              (f))))))

  (testing "should fail when calling non existent function"
    (is (thrown? ExceptionInfo
                 (interpret '((defstackfn func []
                                          (invoke> println 3))
                              (f))))))

  (testing "should fail when calling non existent variable"
    (is (thrown? ExceptionInfo
                 (interpret '((defstackfn f [!a]
                                          !a
                                          !b)
                              (f 100))))))

  (testing "should fail when calling non existent operation"
    (is (thrown? ExceptionInfo
                 (interpret '((defstackfn f [!a]
                                          <some>
                                          !b)
                              (f 100))))))

  (testing "should fail when calling non existent function"
    (is (thrown? ExceptionInfo
                 (interpret '((defstackfn f []
                                          (invoke> not-existing 3))
                              (f))))))

  (testing "should fail if invalid arg count for invoke"
    (is (thrown? ExceptionInfo
                 (interpret '((defstackfn f []
                                          (invoke> println 3))
                              (f))))))

  (testing "should fail if invalid arg count for function"
    (is (thrown? ExceptionInfo
                 (interpret '((defstackfn f [] 1)
                              (f 10 10))))))

  (testing "should fail if pop when empty"
    (is (thrown? ExceptionInfo
                 (interpret '((defstackfn f [] <pop>)
                              (f))))))

  (testing "base case"
    (is (= 24
           (interpret
             '((defstackfn f [!a !b !c]
                           !a
                           !b
                           (invoke> + 2)
                           !v1+
                           !c
                           !c
                           <pop>
                           2
                           (invoke> * 2)
                           !v2+
                           (invoke> = 2)
                           (if>
                             !v1
                             !v2
                             (invoke> - 2)
                             else>
                             "false!!"
                             (invoke> println 1)
                             <pop>
                             !v1
                             !v2
                             (invoke> * 2)))
               (f 1 2 4)))))))
