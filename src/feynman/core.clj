(ns feynman.core
  (:require [feynman.repl :refer [repl]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (repl))
