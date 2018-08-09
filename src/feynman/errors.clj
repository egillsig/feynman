(ns feynman.errors
  (:require [clojure.string :as string]
            [feynman.types :as t]))

(defn pp-var-names
  [n]
  (map #(str "'" (char %)) (range 65 (+ n 65))))

(defn pp-dim
  [d env]
  (letfn [(pp-term [term]
            (let [[base exp] term
                  base (get env base base)]
              (if (= exp 1)
                (str base)
                (str base "^" exp))))]
    (str "[" (string/join " * " (map pp-term d)) "]")))

(def separator
  {:function " -> "
   :product " x "})

(defn pp-type
  [tt]
  (letfn [(pp [t env]
            (cond
              (string? t) t
              (t/base-type? t) (name t)
              (t/variable? t) (get env t t)
              (t/dim-type? t) (pp-dim (second t) env)
              (t/compound-type? t) (str "(" (string/join (separator (first t))
                                                         (map #(pp % env) (rest t)))
                                        ")")))]
    (if (t/type-scheme? tt)
      (let [[_ vars expr] tt]
        (pp expr (zipmap vars (pp-var-names (count vars)))))
      (pp tt {}))))

(defn err-msg
  [msg & args]
  (fn [data]
    (apply format msg (map (comp pp-type data) args))))

(def messages
  {"unknown-var"
   (err-msg "Unknown variable name: %s" :name)
   "unknown-unit"
   (err-msg "Unknown unit name: %s" :name)
   "match-rtn"
   (err-msg "Failed to match return type %s, with inferred type: %s"
            :declared :inferred)
   "match-var"
   (err-msg "Failed to match declared variable type %s, inferred %s" :inferred :declared)
   "match-func-args"
   (err-msg "Failed to match function arguments (%s) with declared type (%s) in expr %s"
            :func-type :args-type :expr)
   "if-not-bool"
   (err-msg "If condition not boolean, type: " :cond-type)
   "match-if-else"
   (err-msg "Failed to match if/else branch types:" :if-type :else-type)})

(defn report
  [exc]
  (let [data (ex-data exc)
        msg (.getMessage exc)
        msg-fn (messages msg)]
    (if msg-fn
      (msg-fn data)
      msg)))
