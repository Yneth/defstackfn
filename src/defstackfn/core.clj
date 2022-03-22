(ns defstackfn.core
  (:require [clojure.test :refer :all]
            [clojure.string :as cstr]
            [defstackfn.util :as util])
  (:import (clojure.lang Symbol IPersistentCollection ExceptionInfo)))

(defonce macro-debug-enabled
  (util/mk-thread-local false))

(defonce runtime-debug-enabled
  (util/mk-thread-local false))

(defn state->stack-push [state val]
  (update state :stack
    (fn [stack]
      (if (= ::nil stack)
        (list val)
        (conj stack val)))))

(defn state->stack-pop [state]
  (update state :stack
    (fn [stack]
      (cond
        (= ::nil stack)
        ::nil

        (= 1 (count stack))
        ::nil

        :else
        (rest stack)))))

(defn state->stack-pop-multi [state n]
  (reduce
    (fn [acc _]
      (state->stack-pop acc))
    state
    (range n)))

(defn state->stack-take [state n]
  (let [stack (get state :stack)]
    (if (= ::nil stack)
      (list)
      (take n stack))))

(defn state->get-stack-head [state]
  (let [stack (get state :stack)]
    (if (= ::nil stack)
      stack
      (first stack))))

(defn state->get-var [state var-name]
  (get-in state [:vars var-name]))

(defn state->define-var [state var-name]
  (let [var-value (state->get-stack-head state)]
    (update state :vars assoc var-name var-value)))

(defmulti ->statement (fn [exp opts] (class exp)))
(defmulti symbol->statement (fn [exp opts] exp))
(defmulti list->statement (fn [exp opts] (first exp)))

(defmethod ->statement Symbol [exp opts]
  (symbol->statement exp opts))

(defmethod ->statement IPersistentCollection [exp opts]
  (list->statement exp opts))

(defmethod ->statement :default [exp opts]
  `(fn [state#]
     (state->stack-push state# ~exp)))

(defmethod symbol->statement '<pop> [exp opts]
  `(fn [state#]
     (let [head# (state->get-stack-head state#)]
       (when (= ::nil head#)
         (throw (ex-info "Failed to pop empty stack" {:exp '~exp :opts '~opts}))))

     (state->stack-pop state#)))

(defn define-var? [sym]
  (let [n (name sym)]
    (and
      (cstr/starts-with? n "!")
      (cstr/ends-with? n "+"))))

(defn push-var? [sym]
  (and
    (cstr/starts-with? (name sym) "!")
    (not (define-var? sym))))

(defn format-var [sym]
  (let [n (name sym)]
    (symbol (subs n 0 (dec (count n))))))

(defmethod symbol->statement :default [exp opts]
  (when-not (or (push-var? exp) (define-var? exp))
    (throw (ex-info (str "Found invalid symbol: " exp) {:exp exp :opts opts})))

  (cond
    (define-var? exp)
    `(fn [state#]
       (state->define-var state# (format-var '~exp)))

    (push-var? exp)
    `(fn [state#]
       (state->stack-push state# (state->get-var state# '~exp)))))

(defmethod list->statement 'invoke> [exp opts]
  (let [[_ f arg-count] exp]
    `(fn [state#]
       (let [args#          (state->stack-take state# ~arg-count)
             updated-state# (state->stack-pop-multi state# ~arg-count)
             result#        (apply ~f args#)]
         (when (< (count args#) ~arg-count)
           (throw (ex-info (str "Stack exhausted, expected to have " ~arg-count
                                " arguments, was " (count args#))
                           {:exp '~exp :opts '~opts})))
         (state->stack-push updated-state# result#)))))

(defn ->logging-statement [exp opts]
  `(fn [state#]
     (println '~exp state# '~opts)
     state#))

(defn do-to-statements [s-exp-list]
  (->> s-exp-list
       (util/scan)
       (mapcat
         (fn [[exp# history#]]
           (let [opts# {:history history#}]
             (when (macro-debug-enabled)
               (println "unfolding statement:" exp# \newline history#))

             (cond-> [(seq [(->statement exp# opts#)])]

                     (runtime-debug-enabled)
                     (conj (seq [(->logging-statement exp# opts#)]))))))))

(defn to-statements [s-exp-list]
  (when (macro-debug-enabled)
    (println "unfold started"))
  (let [result (do-to-statements s-exp-list)]
    (when (macro-debug-enabled)
      (println "unfold completed"))
    result))

(defmethod list->statement 'if> [exp opts]
  (let [body
        (rest exp)

        if-statements
        (to-statements
          (take-while #(not= 'else> %) body))

        else-statements
        (to-statements
          (drop 1 (drop-while #(not= 'else> %) body)))]

    `(fn [state#]
       (let [stack-head#
             (state->get-stack-head state#)

             updated-state#
             (state->stack-pop state#)]
         (cond
           (= ::nil stack-head#)
           (throw (ex-info "Failed to execute if, empty stack"
                           {:exp '~exp :opts '~opts}))

           stack-head#
           (-> updated-state#
               ~@if-statements)

           :else
           (-> updated-state#
               ~@else-statements))))))

(defn format-exception-message [header-message name args-map-or-list ^Exception e]
  (let [{:keys [exp opts]}
        (ex-data e)

        history
        (get opts :history)

        header
        (str header-message name " " args-map-or-list)

        footer
        (str exp \newline
             "^^^^^^^^" \newline
             (.getMessage e))

        message
        (cstr/join
          \newline
          (concat
            [""]
            [header]
            history
            [footer]))]
    message))

(defn format-runtime-exception [name args-map ^ExceptionInfo e]
  (ex-info (format-exception-message "Failed to invoke: " name args-map e) {} e))

(defn format-compile-exception [name args ^ExceptionInfo e]
  (ex-info (format-exception-message "Failed to compile: " name args e) {}))

(defmacro defstackfn [& body]
  (let [[name args & s-exps] body]
    (try
      `(defn ~name ~args
         (let [args-map# (zipmap '~args ~args)]
           (try
             (->
               {:stack ::nil
                :vars  args-map#}
               ~@(to-statements s-exps)
               (state->get-stack-head))
             (catch ExceptionInfo e#
               (throw (format-runtime-exception '~name args-map# e#)))
             (catch Exception e#
               (throw (IllegalStateException. "Unhandled exception" e#))))))
      (catch ExceptionInfo e
        (throw (format-compile-exception name args e))))))
