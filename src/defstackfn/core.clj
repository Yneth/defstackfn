(ns defstackfn.core
  (:require [clojure.string :as cstr]
            [defstackfn.util :as util])
  (:import (java.io PushbackReader StringReader)))

(defonce ^:private debug
  (util/mk-thread-local false))

(defn- literal? [exp]
  (or
    (number? exp)
    (string? exp)
    (boolean? exp)
    (nil? exp)))

(defn- stack-pop? [exp]
  (= '<pop> exp))

(defn- stack-push-var? [exp]
  (and
    (symbol? exp)
    (let [n (name exp)]
      (and (cstr/starts-with? n "!")
           (not (cstr/ends-with? n "+"))))))

(defn- define-var? [exp]
  (and
    (symbol? exp)
    (let [n (name exp)]
      (and (cstr/starts-with? n "!")
           (cstr/ends-with? n "+")))))

(defn- invoke? [exp]
  (and
    (coll? exp)
    (= 'invoke> (first exp))))

(defn- invoke-cljfn? [exp]
  (and
    (invoke? exp)
    (let [[_ fname] exp]
      (if (and (coll? fname) (= 'fn* (first fname)))
        (throw (ex-info "Anonymous functions are not supported" {}))
        (some? (resolve fname))))))

(defn- invoke-stackfn? [exp]
  (and
    (invoke? exp)
    (let [[_ fname] exp]
      (symbol? fname))))

(defn- >empty? [vstack!]
  (empty? @vstack!))

(defn- >push [vstack! v]
  (vswap! vstack! #(cons v %)))

(defn- >peek [vstack!]
  (first @vstack!))

(defn- >pop [vstack!]
  (when (>empty? vstack!)
    (throw (ex-info "Failed to pop, empty stack" {})))

  (let [val (>peek vstack!)]
    (vswap! vstack! #(drop 1 %))
    val))

(defn- >pop-safe [vstack!]
  (when (>peek vstack!)
    (>pop vstack!)))

(defn- validate-invoke [vstack! exp]
  (when-not (nth exp 1 nil)
    (throw (ex-info "Failed to invoke, unknown function name: " exp {})))

  (when-not (nth exp 2 nil)
    (throw (ex-info "Failed to invoke, invalid argument count: " exp {})))

  (when-not (>= (count @vstack!) (nth exp 2))
    (throw (ex-info (str "Not enough values in stack to invoke: " (second exp)
                         ", needed " (nth exp 2) " was " (count @vstack!)) {}))))

(defn- >invoke-clj-fn [vstack! exp]
  (validate-invoke vstack! exp)
  (let [[_ fsymbol arg-count] exp

        resolved-clj-fn (resolve fsymbol)
        args            (take arg-count @vstack!)]

    (dotimes [_ arg-count] (>pop vstack!))
    (>push vstack! (apply resolved-clj-fn args))))

(defn resolve-fn [vctx! fsymbol]
  (let [res (get @vctx! fsymbol)]
    (when-not res
      (throw (ex-info (str "Encountered non-existent function: " fsymbol) {})))
    res))

(defn- resolve-var [vvars! params exp]
  (let [local-var (get @vvars! exp ::nil)
        fn-var    (get params exp ::nil)]

    (when (and (= ::nil local-var) (= ::nil fn-var))
      (throw (ex-info (str "Failed to resolve var: " exp) {})))

    (if (not= ::nil local-var)
      local-var
      fn-var)))

(defn- >invoke-stack-fn [vstack! interp-fn vctx! exp]
  (validate-invoke vstack! exp)
  (let [[_ fsymbol arg-count] exp

        resolved-stack-fn (resolve-fn vctx! fsymbol)

        param-keys        (get resolved-stack-fn :params)
        param-vals        (take arg-count @vstack!)
        param-map         (util/to-map param-keys param-vals)
        result            (interp-fn param-map vctx! (:body resolved-stack-fn))]
    (>push vstack! result)))

(defn- >define-var [vvars! vstack! exp]
  (let [var-name (cstr/replace (name exp) #"\+$" "")]
    (vswap! vvars! assoc (symbol var-name) (>peek vstack!))))

(defn- if? [exp]
  (and
    (coll? exp)
    (= 'if> (first exp))))

(defn- else? [exp]
  (= 'else> exp))

(defn- try-add-next-call-stack [vcall-stack! type vnext!]
  (when @vnext!
    (>push vcall-stack! {:type type :next @vnext!})))

(defn- interpret-function-body [params vctx! s-exp]
  (let [this           interpret-function-body

        vvars!         (volatile! {})
        vstack!        (volatile! [])
        vcall-stack!   (volatile! [])
        vnext-exp!     (volatile! nil)

        do-resolve-var (partial resolve-var vvars! params)

        set-next!      (fn [next]
                         (let [next-or-eof
                               (if (empty? next) [::eof] #_:else next)]
                           (vreset! vnext-exp! next-or-eof)))]
    (loop [s-exp-iter s-exp]
      (let [exp (first s-exp-iter)]

        (when-not (= ::eof exp)
          (set-next! (rest s-exp-iter)))

        (cond
          (= ::eof exp) nil
          (literal? exp) (>push vstack! exp)
          (stack-push-var? exp) (>push vstack! (do-resolve-var exp))
          (stack-pop? exp) (>pop vstack!)
          (define-var? exp) (>define-var vvars! vstack! exp)
          (invoke-cljfn? exp) (>invoke-clj-fn vstack! exp)
          (invoke-stackfn? exp) (>invoke-stack-fn vstack! this vctx! exp)

          (if? exp)
          (if (>pop vstack!)
            (do
              (try-add-next-call-stack vcall-stack! :if-branch vnext-exp!)
              (set-next! (rest exp)))
            #_:else
            (do
              (try-add-next-call-stack vcall-stack! :else-branch vnext-exp!)
              (let [else-exp (drop-while (complement else?) exp)]
                (set-next! (rest else-exp)))))

          (else? exp)
          (cond
            (>empty? vcall-stack!)
            (throw (ex-info "Else expression without if" {}))

            (= :if-branch (:type (>peek vcall-stack!)))
            (set-next! (:next @vcall-stack!)))


          :else (throw (ex-info (str "Unknown expression: " exp) {})))

        (if (and (debug) (not= ::eof exp))
          (prn exp @vstack!))

        (cond
          (not (= ::eof exp))
          (recur @vnext-exp!)

          (not (>empty? vcall-stack!))
          (recur (:next (>pop-safe vcall-stack!)))

          :else
          nil)))

    (>peek vstack!)))

(defn- register-defstackfn [v-fn-ctx! s-exp]
  (try
    (let [[name params & rest-s-exp] s-exp]
      (when-not (symbol? name)
        (throw (ex-info (str "Invalid function name: " name)
                        {:exp s-exp :ctx v-fn-ctx!})))

      (when-not (coll? params)
        (throw (ex-info (str "Invalid function arguments, function: " name ", args: " params)
                        {:exp s-exp :ctx v-fn-ctx!})))

      (vswap! v-fn-ctx! assoc
              name
              {:name   name
               :params params
               :body   rest-s-exp}))
    (catch Exception e
      (throw (ex-info "Failed to register function" {} e)))))

(defn- interpret-function-call [vfn-ctx! fname param-vals]
  (let [resolved-f (resolve-fn vfn-ctx! fname)
        param-keys (:params resolved-f)]

    (when-not (= (count param-vals) (count param-keys))
      (throw (ex-info (str "Invalid argument count, function: " fname ", expected: "
                           (count param-keys) " was " (count param-vals)) {})))

    (try
      (interpret-function-body
        (util/to-map param-keys param-vals)
        vfn-ctx!
        (:body resolved-f))
      (catch Exception e
        (throw (ex-info (str "Failure during invocation of function: " fname) {} e))))))

(defn- do-interpret [s-exp]
  (let [vfn-ctx!      (volatile! {})
        vcall-stack!  (volatile! [])
        vlast-result! (volatile! nil)]
    (loop [exp-iter s-exp]
      (let [curr  (first exp-iter)
            other (rest exp-iter)]
        (cond
          (nil? curr)
          @vlast-result!

          ; interpret defstackfn or function call
          (coll? curr)
          (do
            (>push vcall-stack! other)
            (recur curr))

          (= curr 'defstackfn)
          (do
            (vreset! vlast-result! (:name (register-defstackfn vfn-ctx! other)))
            (recur (>pop-safe vcall-stack!)))

          :else
          (do
            (vreset! vlast-result! (interpret-function-call vfn-ctx! curr other))
            (recur (>pop-safe vcall-stack!))))))))

(defn interpret [s-exp & [{:keys [debug?]
                           :or   {debug? false}}]]
  (debug :set debug?)
  (try
    (do-interpret s-exp)
    (catch Exception e
      (throw (ex-info "Failed to interpret" {} e)))))

(defn read-all-s-exps [str]
  (let [rdr (PushbackReader. (StringReader. str))]
    (take-while
      #(not= % ::eof)
      (repeatedly #(read {:eof ::eof} rdr)))))

(defn interpret-str [str & [opts]]
  (interpret (read-all-s-exps str) opts))
