(ns feynman.parser
  (:require [clojure.string :as string]
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

(def parse-numbers (partial insta/transform
                            {:number clojure.edn/read-string}))

(defn strip-enlive
  "Strip all useless levels of tree nesting from expression e"
  [e]
  (let [content (:content e)]
    (cond (not (coll? e)) e
          (= (count content) 1)
          (let [f (first content)] (if (coll? f) (strip-enlive f) e))
          :else (assoc e :content (map strip-enlive content)))))

(def parse #(-> % preprocess parser #_strip-enlive))
