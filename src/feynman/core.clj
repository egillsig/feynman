(ns feynman.core
  (:require [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [feynman.env :as env]
            [feynman.errors :as err]
            [feynman.infer :as i]
            [feynman.parser :as p]
            [feynman.transpile :refer [transpile]]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(defn process-input
  [env input opts]
  (try
    (let [parsed (p/parse input)
          [e-name e-type unit?] (i/infer-type env parsed)
          code (transpile parsed)]
      (flush)
      (when-not (:no-types opts)
        (println "Inferred type:" (err/pp-type e-type)))
      (when-not (:no-generate opts)
        (pprint code))
      (when (:eval opts) (println "Evaluated code:" (eval code)))
      (flush)
      [e-name e-type unit?])
    (catch clojure.lang.ExceptionInfo e
      (println (err/report e)))))

(defn process-inputs
  ([inputs opts]
   (loop [env env/init-types i inputs]
     (when i
       (let [input (string/trim (first i))]
         (if (= "" input)
           (recur env (next i))
           (do
             (when-not (:no-input opts) (println "> " input))
             (flush)
             (let [[e-name e-type unit?] (process-input env input opts)]
               (if (nil? e-name)
                 (recur env (next i))
                 (if unit?
                   (recur (-> env
                              (assoc e-name e-type)
                              (assoc-in [:types e-name] e-type))
                          (next i))
                   (recur (assoc env e-name e-type) (next i))))))))))))

(defn get-input
  []
  (loop [input nil]
    (print (if input "  " "> "))
    (flush)
    (let [rd (read-line) input (str input " " rd)]
      (when rd
        (if-not (clojure.string/ends-with? input ";;")
          (recur input)
          input)))))

(defn repl [] (process-inputs (repeatedly get-input) {:no-input true}))

(def cli-options
  [[nil "--no-input" "Don't print input"]
   [nil "--no-types" "Don't print type information"]
   [nil "--no-generate" "Don't print generated code"]
   [nil "--eval" "Evaluate generated code"]])

(defn -main
  [& args]
  (let [opts (parse-opts args cli-options)]
    (if (:errors opts)
      (println (string/join \newline (:errors opts)))
      (let [inputs (if (:arguments opts)
                     (-> opts :arguments first slurp (string/split #";;"))
                     (repeatedly get-input))]
        (process-inputs inputs (:options opts))))))
