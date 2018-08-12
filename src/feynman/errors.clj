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
              (t/compound-type? t)
              (let [s (string/join (separator (first t))
                                   (map #(pp % env) (rest t)))]
                (if (= :function (first t))
                  s
                  (str "(" s ")")))))]
    (if (t/type-scheme? tt)
      (let [[_ vars expr] tt]
        (pp expr (zipmap vars (pp-var-names (count vars)))))
      (let [vars (t/free-vars tt)]
        (pp tt (zipmap vars (pp-var-names (count vars))))))))

(defn err-msg
  [msg & args]
  (fn [data]
    (apply format msg (map (comp pp-type data) args))))

(defn handle-unknown-var
  [data]
  (string/join
   \newline
   [(format "Unknown variable name: %s" (:name data))]))

(defn handle-var-type
  [data]
  (string/join
   \newline
   ["Failed to match declared type:"
    (format "\t%s" (pp-type (:declared data)))
    "with inferred type:"
    (format "\t%s" (pp-type (:inferred data)))]))

(defn handle-args-type
  [data]
  (string/join
   \newline
   ["Failed to match type of function arguments:"
    (format "\t%s" (pp-type (:func-type data)))
    "given type:"
    (format "\t%s" (pp-type (:args-type data)))]))

(def messages
  {"unknown-var" handle-unknown-var
   "unknown-unit" handle-unknown-var
   "match-rtn" handle-var-type
   "match-var" handle-var-type
   "match-func-args" handle-args-type
   "if-not-bool" (err-msg "If condition not boolean, type: %s" :cond-type)
   "match-if-else"
   (err-msg "Failed to match types of if/else branches:\n\tif:%s\n\telse:%s"
            :if-type :else-type)})

(defn report
  [exc]
  (let [data (ex-data exc)
        msg (.getMessage exc)
        msg-fn (messages msg)]
    (if msg-fn
      (msg-fn data)
      msg)))
