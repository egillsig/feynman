(ns feynman.parser
  (:require [clojure.string :as string]
            [instaparse.core :as insta]))

(defn remove-comments [s] (string/replace s #"#.*" ""))

(def parser (insta/parser "src/feynman/grammar.bnf"
                          :auto-whitespace :standard
                          :output-format :hiccup))

(defn callfunc
  [fname]
  (fn [& args] (apply vector :apply [:name fname] args)))

(defn infix->prefix
  [a op b] [:apply op a b])

(def transform-ops
  "Transform operators to function calls"
  (partial insta/transform
           {:add (callfunc "+")
            :subtract (callfunc "-")
            :mul (callfunc "*")
            :div (callfunc "/")
            :exp (callfunc "^")
            :logic-op #(vector :name %)
            :logic infix->prefix}))

(defn throw-if-fail
  [e]
  (if (instance? instaparse.gll.Failure e)
    (throw (ex-info "Parse error" {:error e}))
    e))

(defn num-parser [kw] (fn [n] [kw (clojure.edn/read-string n)]))
(def parse-numbers (partial insta/transform
                            {:number (num-parser :number)
                             :integer (num-parser :integer)}))

(def transform-expr (comp parse-numbers transform-ops))

(def parse (comp transform-expr first throw-if-fail parser remove-comments))
