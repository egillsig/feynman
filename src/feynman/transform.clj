(ns feynman.transform
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as walk]))

(defn transform-node
  [node]
  (match [node]
    [[(:or :expr :a_expr :m_expr) a op b]] [:apply op [:argument-list a b]]
    [[(:or :logic-op :a_op :m_op) op-name]] [:primary [:name op-name]]
    [[:m_op]] [:name "*"]
    :else node))

(defn transform-tree
  [expr]
  (walk/postwalk transform-node expr))
