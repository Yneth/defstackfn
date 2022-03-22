(ns defstackfn.core-test
  (:require [clojure.test :refer :all]
            [defstackfn.core :refer :all]
            [clojure.string :as cstr])
  (:import (clojure.lang ArityException ExceptionInfo)))

; (runtime-debug-enabled :set true)

(deftest literal-tests
  (testing "should return const head"
    (defstackfn const1 [] 1)
    (is (= 1 (const1))))

  (testing "should return const head"
    (defstackfn const-str [] "1")
    (is (= "1" (const-str)))))

(deftest arg-tests
  (testing "should accept args"
    (defstackfn const100 [!a] !a)
    (is (= 100 (const100 100))))

  (testing "should fail if invalid arg count"
    (defstackfn >tuple [!a !b]
                !b
                !a
                (invoke> vector 2))
    (is (thrown? ArityException (>tuple 1)))))

(deftest pop-tests
  (testing "should pop"
    (defstackfn >second [!a !b] !a <pop> !b)
    (is (= "second" (>second "first" "second"))))

  (testing "should fail if pop when empty"
    (defstackfn >nil [] 1 <pop>)
    (is (= :defstackfn.core/nil (>nil))))

  (testing "should fail if pop when empty"
    (defstackfn >invalid [] <pop>)
    (is (thrown? ExceptionInfo (>invalid)))))

(deftest invoke-tests
  (testing "should invoke function and return its value"
    (defstackfn >sum [!a !b] !a !b (invoke> + 2))
    (is (= 16 (>sum 8 8))))

  (testing "should invoke other defstackfn function"
    (defstackfn const1 [] 1)
    (defstackfn >inc [!a]
                (invoke> const1 0)
                !a
                (invoke> + 2))
    (is (= 2 (>inc 1))))

  (testing "should fail if invalid arg count for invoke"
    (defstackfn >tuple [!a !b]
                !b
                !a
                (invoke> vector 3))
    (is (thrown-with-msg?
          ExceptionInfo
          #"\(invoke> vector 3\)"
          (>tuple 1 2)))))

(deftest var-define-tests
  (testing "should support var define"
    (defstackfn >mul4 [!a]
                !a
                !a
                (invoke> + 2)
                !b+
                !b
                (invoke> + 2))
    (is (= 8 (>mul4 2))))

  (testing "should shadow function args"
    (defstackfn >const100 [!a]
                !a
                100
                !a+
                !a)
    (is (= 100 (>const100 2))))

  (testing "should shadow function args inside if"
    (defstackfn >const100 [!a]
                !a
                (if> 100 !a+)
                !a)
    (is (= 100 (>const100 2)))))

(deftest if-tests
  (testing "should fail if empty stack"
    (defstackfn >if-fail []
                (if> "if-branch" else> "else-branch"))
    (is (thrown? ExceptionInfo (= "if-branch" (>if-fail)))))

  (testing "should invoke if branch"
    (defstackfn >eq [!a !b]
                !a
                !b
                (invoke> = 2)
                (if> "if-branch" else> "else-branch"))
    (is (= "if-branch" (>eq 8 8))))

  (testing "should invoke else branch"
    (defstackfn >eq [!a !b]
                !a
                !b
                (invoke> = 2)
                (if> "if-branch" else> "else-branch"))
    (is (= "else-branch" (>eq 8 10))))

  (testing "should support nested ifs"
    (defstackfn >even-and-pos [!a]
                !a
                !a
                (invoke> even? 1)
                (if>
                  (invoke> pos? 1)
                  (if> true else> false)
                  else>
                  false))
    (is (= true (>even-and-pos 10)))
    (is (= false (>even-and-pos -10)))
    (is (= false (>even-and-pos 1)))))

(deftest error-handling-tests
  (testing "should fail if invalid arg count"
    (defstackfn >tuple [!a !b]
                !b
                !a
                (invoke> vector 2))
    (is (thrown? ArityException (>tuple 1)))))

(deftest integration-test
  (testing "base case"
    (defstackfn f [!a !b !c]
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
    (is (= 24 (f 1 2 4)))))


(deftest errors-test
  ; uncomment to check error message
  #_(defstackfn >unknown [] <pop> <test>)

  (testing "check error message"
    (defstackfn >tuple [!a !b]
                !b
                !a
                (invoke> vector 3))
    (try
      (>tuple 1 2)
      (catch Exception e
        (is (= (cstr/join
                 \newline
                 [""
                  "Failed to invoke: >tuple {!a 1, !b 2}"
                  "!b"
                  "!a"
                  "(invoke> vector 3)"
                  "^^^^^^^^"
                  "Stack exhausted, expected to have 3 arguments, was 2"])
               (.getMessage e)))))))
