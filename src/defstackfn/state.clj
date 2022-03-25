(ns defstackfn.state
  (:import (clojure.lang IPersistentList IPersistentMap Symbol)))

(defrecord State [^IPersistentList stack ^IPersistentMap vars]
  Object
  (toString [this]
    (str "{:stack " stack ", :vars " vars "}")))

(defn init-state [vars]
  (State. ::nil vars))

(defn ^State stack-push [^State state val]
  (update state :stack
    (fn [stack]
      (if (= ::nil stack)
        (list val)
        (conj stack val)))))

(defn ^State stack-pop [^State state]
  (update state :stack
    (fn [stack]
      (cond
        (= ::nil stack)
        ::nil

        (= 1 (count stack))
        ::nil

        :else
        (rest stack)))))

(defn ^State stack-pop-multi [^State state n]
  (reduce
    (fn [acc _]
      (stack-pop acc))
    state
    (range n)))

(defn ^IPersistentList stack-take [^State state n]
  (let [stack (get state :stack)]
    (if (= ::nil stack)
      (list)
      (take n stack))))

(defn ^Object get-stack-head [^State state]
  (let [stack (get state :stack)]
    (if (= ::nil stack)
      stack
      (first stack))))

(defn stack-empty? [^State state]
  (= ::nil (get state :stack)))

(defn ^Object get-var [^State state ^Symbol var-name or-else]
  (get-in state [:vars var-name] or-else))

(defn ^State define-var [^State state ^Symbol var-name]
  (let [var-value (get-stack-head state)]
    (update state :vars assoc var-name var-value)))
