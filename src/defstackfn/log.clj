(ns defstackfn.log
  (:require [defstackfn.util :as util]))

(defonce macro-debug-enabled
  (util/mk-thread-local false))

(defonce runtime-debug-enabled
  (util/mk-thread-local false))
