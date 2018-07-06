(ns feynman.parser
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [feynman.infer :as i]
            [instaparse.core :as insta]))

(defn preprocess
  "Remove comments and newlines"
  [s]
  (-> s
      (string/replace #"#.*" "")
      (string/replace #"\n" "")))

(def parser (insta/parser "src/feynman/grammar.bnf"
                          :auto-whitespace :standard
                          :output-format :hiccup))
(defn parse
  [e]
  (let [parsed (insta/parses parser e)]
    (cond (= 0 (count parsed))
          (throw (ex-info "Unable to parse expression"
                          {:expression e :failure (meta parsed)}))
          (> 1 (count parsed))
          (throw (ex-info "Ambiguous expression"
                          {:expression e :parses parsed}))
          :else (first parsed))))

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

(def init-types
  {"*" [:forall [[:dim-variable 1] [:dim-variable 2]]
        [:function [:product
                    [:dimension {[:dim-variable 1] 1}]
                    [:dimension {[:dim-variable 2] 1}]]
         [:dimension {[:dim-variable 1] 1 [:dim-variable 2] 1}]]]

   "/" [:forall [[:dim-variable 1] [:dim-variable 2]]
        [:function [:product
                    [:dimension {[:dim-variable 1] 1}]
                    [:dimension {[:dim-variable 2] 1}]]
         [:dimension {[:dim-variable 1] 1 [:dim-variable 2] -1}]]]

   "+" [:forall [[:dim-variable 1]]
        [:function [:product
                    [:dimension {[:dim-variable 1] 1}]
                    [:dimension {[:dim-variable 1] 1}]]
         [:dimension {[:dim-variable 1] 1}]]]

   "-" [:forall [[:dim-variable 1]]
        [:function [:product
                    [:dimension {[:dim-variable 1] 1}]
                    [:dimension {[:dim-variable 1] 1}]]
         [:dimension {[:dim-variable 1] 1}]]]

   ;; Note these should be defined in a library written in Feynman itself
   "m" [:dimension {"L" 1}]
   "s" [:dimension {"T" 1}]
   "kg" [:dimension {"M" 1}]
   "N" [:dimension {"L" 1 "T" -2 "M" 1}]})

(defn infer-expr
  [e]
  (->> e
       parser
       transform-tree
       (i/infer init-types)))

#_(defn run-program
    [p]
    (loop [stmts (rest p) type-env init-types]
      (let [s (first stmts)]
        (if (= :definition (first s))
          (recur (rest stmts)
                 (assoc type-env
                        (def-name s)
                        (i/infer type-env (def-expr s))))
          (do
            (println (i/infer type-env s))
            (recur (rest stmts) type-env))))))
