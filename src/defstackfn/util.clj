(ns defstackfn.util
  (:import (java.util.function Supplier)))

(defn mk-thread-local [& [init-val]]
  (let [thread-local
        (ThreadLocal/withInitial
          (reify Supplier (get [_] init-val)))

        this
        (fn [& [cmd v]]
          (case cmd
            :get (.get thread-local)
            :set (do (.set thread-local v) v)
            (.get thread-local)))]
    this))

(defn scan [s-exp-list]
  (if-not (empty? s-exp-list)
    (reductions
      (fn [[exp history :as acc] x]
        [x (conj history exp)])
      [(first s-exp-list) []]
      (next s-exp-list))
    #_else
    []))
