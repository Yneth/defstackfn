(ns defstackfn.error
  (:require [clojure.string :as cstr])
  (:import (clojure.lang Symbol IPersistentMap IPersistentList ExceptionInfo)))

(defn format-exception-message
  [^String header-message
   ^Symbol function-name
   args-map-or-list
   ^Exception e]
  (let [{:keys [exp opts state]}
        (ex-data e)

        history
        (get opts :history)

        header
        (str header-message function-name " " args-map-or-list)

        footer
        (cond->
          (str exp \newline
               "^^^^^^^^" \newline
               (.getMessage e))

          state
          (str \newline "State: " state))

        message
        (cstr/join
          \newline
          (concat
            [""]
            [header]
            history
            [footer]))]
    message))

(defn- defstackfn-error? [e]
  (some? (:exp (ex-data e))))

(defn map-exception [e ^ExceptionInfo or-else-fn]
  (if (not (defstackfn-error? e))
    e
    #_:else
    (or-else-fn)))

(defn try-format-runtime-exception [^Symbol name ^IPersistentMap args-map ^ExceptionInfo e]
  (map-exception e #(ex-info (format-exception-message "Failed to invoke: " name args-map e) {} e)))

(defn try-format-compile-exception [^Symbol name ^IPersistentList args ^ExceptionInfo e]
  (map-exception e #(ex-info (format-exception-message "Failed to compile: " name args e) {})))
