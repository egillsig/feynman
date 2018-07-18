(ns feynman.parser
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [feynman.infer :as i]
            [instaparse.core :as insta]))

(defn preprocess
  "Remove comments"
  [s]
  (string/replace s #"#.*" ""))

(def parser (insta/parser "src/feynman/grammar.bnf"
                          :auto-whitespace :standard
                          :output-format :hiccup))

(defn callfunc
  [fname]
  (fn [& args] (apply vector :apply [:name fname] args)))

(def transform-ops
  "Transform operators to function calls"
  (partial insta/transform
           {:add (callfunc "+")
            :subtract (callfunc "-")
            :mul (callfunc "*")
            :div (callfunc "/")}))

(defn throw-if-fail
  [e]
  (if (instance? instaparse.gll.Failure e)
    (throw (ex-info "Parse error"
                    {:error e}))
    e))

#_(def parse-numbers (partial insta/transform {:number clojure.edn/read-string}))

(def transform-expr (comp #_parse-numbers transform-ops))

(def parse (comp transform-expr first throw-if-fail parser preprocess))
