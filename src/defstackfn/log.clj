(ns defstackfn.log
  (:require [defstackfn.util :as util]))

(def macro-debug-enabled
  (util/mk-thread-local false))

(def runtime-debug-enabled
  (util/mk-thread-local false))
