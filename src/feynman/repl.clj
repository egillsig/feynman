(ns feynman.repl
  (:require [feynman.env :as env]
            [feynman.errors :as err]
            [feynman.infer :as i]
            [feynman.parser :as p]
            [feynman.transpile :refer [transpile]]))

(defn process-input
  [env input]
  (when-not
   (re-matches #"\W+" input)
    (try
      (let [parsed (p/parse input)]
        [(i/infer-type env parsed) (transpile parsed)])
      (catch clojure.lang.ExceptionInfo e
        (println (err/report e))))))

(defn process-inputs
  ([inputs] (process-inputs
             inputs
             {:eval true :input true :type true :generated true}))
  ([inputs opts]
   (loop [env env/init-types i inputs]
     (when i
       (when-let [input (first i)]
         (if (= "" input)
           (recur env (next i))
           (do
             (when (:input opts) (println "> " input))
             (flush)
             (let [[[e-name e-type unit?] code] (process-input env input)]
               (flush)
               (when (:type opts) (println "Inferred type:" (err/pp-type e-type)))
               (when (:generated opts) (println "Generated code:" code))
               (when (:eval opts) (println "Evaluated code:" (eval code)))
               (flush)
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

(defn repl [] (process-inputs (repeatedly get-input)))
