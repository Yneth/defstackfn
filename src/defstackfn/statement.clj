(ns defstackfn.statement
  (:require [clojure.string :as cstr]
            [defstackfn.state :as state]
            [defstackfn.util :as util]
            [defstackfn.log :as log]
            [taoensso.timbre :as timbre])
  (:import (clojure.lang IFn IPersistentList Symbol)
           (defstackfn.state State)))

(defprotocol Statement
  (invoke [this ^State state]))

(deftype StackPopStatement [exp opts]
  IFn
  Statement
  (invoke [_ state]
    (when (state/stack-empty? state)
      (throw (ex-info "Failed to pop empty stack"
                      {:exp exp :opts opts :state state})))
    (state/stack-pop state)))

(deftype StackPushLiteralStatement [exp opts]
  Statement
  (invoke [_ state]
    (state/stack-push state exp)))

(deftype StackPushVarStatement [exp opts]
  Statement
  (invoke [_ state]
    (let [var-value (state/get-var state exp ::nil)]
      (when (= ::nil var-value)
        (throw (ex-info (str "Unknown variable: " exp) {:exp exp :opts opts})))
      (state/stack-push state var-value))))

(defn define-var? [^Symbol sym]
  (let [n (name sym)]
    (and
      (cstr/starts-with? n "!")
      (cstr/ends-with? n "+"))))

(defn push-var? [^Symbol sym]
  (and
    (cstr/starts-with? (name sym) "!")
    (not (define-var? sym))))

(defn format-var [^Symbol sym]
  (let [n (name sym)]
    (symbol (subs n 0 (dec (count n))))))

(deftype DefineVarStatement [exp opts]
  Statement
  (invoke [_ state]
    (state/define-var state (format-var exp))))

(deftype InvokeStatement [exp opts f arg-count]
  Statement
  (invoke [_ state]
    (let [args          (state/stack-take state arg-count)
          updated-state (state/stack-pop-multi state arg-count)]

      (when-not f
        (throw (ex-info (str "No such function found: " (second exp))
                        {:exp exp :opts opts :state state})))

      (when (< (count args) arg-count)
        (throw (ex-info (str "Stack exhausted, expected to have " arg-count
                             " arguments, was " (count args))
                        {:exp exp :opts opts :state state})))
      (state/stack-push updated-state (apply f args)))))

(deftype Statements [stmts]
  Statement
  (invoke [_ state]
    (reduce
      (fn [^State acc stmt]
        (.invoke stmt acc))
      state
      stmts)))

(deftype IfStatement [exp opts if-statements else-statements]
  Statement
  (invoke [_ state]
    (when (state/stack-empty? state)
      (throw (ex-info "Failed to execute if>, empty stack"
                      {:exp exp :opts opts :state state})))
    (let [updated-state (state/stack-pop state)]
      (if (state/get-stack-head state)
        (.invoke if-statements updated-state)
        (.invoke else-statements updated-state)))))

(deftype LoggingStatement [exp opts]
  Statement
  (invoke [_ state]
    (when (log/runtime-debug-enabled)
      (timbre/info exp state opts))
    state))

(defmulti ->statement (fn [exp opts] (class exp)))
(defmulti symbol->statement (fn [^Symbol exp opts] exp))
(defmulti list->statement (fn [^IPersistentList exp opts] (first exp)))

(defmethod ->statement Symbol [exp opts]
  (symbol->statement exp opts))

(defmethod ->statement IPersistentList [exp opts]
  (list->statement exp opts))

(defmethod ->statement :default [exp opts]
  (StackPushLiteralStatement. exp opts))

(defmethod symbol->statement '<pop> [^Symbol exp opts]
  (StackPopStatement. exp opts))

(defmethod symbol->statement :default [^Symbol exp opts]
  (when-not (or (push-var? exp) (define-var? exp))
    (throw (ex-info (str "Found invalid symbol: " exp) {:exp exp :opts opts})))

  (cond
    (define-var? exp)
    (DefineVarStatement. exp opts)

    (push-var? exp)
    (StackPushVarStatement. exp opts)))

(defmethod list->statement 'invoke> [exp opts]
  (let [[_ f arg-count] exp]
    (when-not (symbol? f)
      (throw (ex-info (str "invalid function name: " f) {:exp exp :opts opts})))

    (when-not (and arg-count (or (zero? arg-count) (pos-int? arg-count)))
      (throw (ex-info (str "invalid arg count: " arg-count) {:exp exp :opts opts})))

    (InvokeStatement. exp opts (resolve f) arg-count)))

(defmethod list->statement :default [exp opts]
  (throw (ex-info (str "unknown list exp: " (first exp)) {:exp exp :opts opts})))

(defn do-to-statements
  ([s-exp-list]
   (do-to-statements s-exp-list []))
  ([s-exp-list par-history]
   (->> s-exp-list
        (util/scan)
        (mapcat
          (fn [[exp# history#]]
            (when (log/macro-debug-enabled)
              (timbre/debug exp#))

            (let [opts# {:history (concat par-history history#)}]
              (cond->
                [(->statement exp# opts#)]

                (log/runtime-debug-enabled)
                (conj (LoggingStatement. exp# opts#))))))
        (doall))))

(defmethod list->statement 'if> [exp opts]
  (let [body
        (rest exp)

        [if-statements _ else-statements]
        (partition-by #(not= 'else> %) body)

        history
        (:history opts)]
    (IfStatement.
      exp
      opts
      (Statements. (do-to-statements if-statements history))
      (Statements. (do-to-statements else-statements history)))))

(defn to-statements [s-exp-list]
  (when (log/macro-debug-enabled) (timbre/debug "unfold started"))
  (let [result (do-to-statements s-exp-list)]
    (when (log/macro-debug-enabled) (timbre/debug "unfold completed"))
    (Statements. result)))
