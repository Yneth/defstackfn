(ns defstackfn.core
  (:require [defstackfn.state :as state]
            [defstackfn.error :as error]
            [defstackfn.statement :as statement])
  (:import (clojure.lang ExceptionInfo)))

(defmacro defstackfn [& body]
  (let [[name args & s-exps] body
        result-fn (statement/to-statements s-exps)]
    (try
      `(defn ~name ~args
         (let [args-map# (zipmap '~args ~args)
               state#    (state/init-state args-map#)]
           (try
             (-> (.invoke ~result-fn state#)
                 (state/get-stack-head))
             (catch ExceptionInfo e#
               (throw (error/try-format-runtime-exception '~name args-map# e#))))))
      (catch ExceptionInfo e
        (throw (error/try-format-compile-exception name args e))))))
