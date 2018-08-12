(ns feynman.transpile
  (:require [clojure.math.numeric-tower :refer [expt]]
            [instaparse.core :as insta]))

(def name-map
  {"==" '=
   "^" `expt})

(defn transform-let
  [dfn body]
  (if (= (first dfn) `defn)
    (let [[n params fn-body] (rest dfn)]
      `(let [~n (fn ~params ~fn-body)] ~body))
    `(let ~(vec (rest dfn)) ~body)))

(defn transform-funcdef
  ([nm args body] `(defn ~nm ~args ~body))
  ([nm args ret-type body] `(defn ~nm ~args ~body)))

(def transform-map
  {:apply (fn [& args] (sequence args))
   :name (fn [n] (symbol (get name-map n n)))
   :boolean #(if (= % "True") true false)
   :number identity
   :def (fn [& args] `(def ~@args))
   :unit-def (fn [n & args] `(def ~n 1))
   :arg (fn [& args] (first args))
   :arg-list vector
   :func-def transform-funcdef
   :if (fn [& args] `(if ~@args))
   :function (fn [args expr] `(fn ~args ~expr))
   :let transform-let})

(def transpile (partial insta/transform transform-map))
