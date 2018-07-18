(ns feynman.repl
  (:require [feynman.env :as env]
            [feynman.infer :as i]
            [feynman.parser :as p]))

;; TODO: Error reporting
(def report identity)

(defn process-input
  [env input]
  (try
    (->> input
         p/parse
         (i/infer-type env))
    (catch clojure.lang.ExceptionInfo e
      (print (report e)))))

(defn repl []
  (loop [tenv env/init-types input nil]
    (print (if input " " ">"))
    (flush)
    (let [env env/init-types input (str (or input "") (read-line))]
      (when input
        (if-not (clojure.string/ends-with? input ";;")
          (recur env input)
          (let [[e-name e-type] (process-input env input)]
            (println e-type)
            (if (nil? e-name)
              (recur env nil)
              (recur (assoc env e-name e-type) nil))))))))
