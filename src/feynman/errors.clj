(ns feynman.errors
  (:require [clojure.string :as string]
            [feynman.dimensions :refer [create-vec]]
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

;;; Matrix operations for dimensional analysis

; (s/def ::matrix-entry rational?)
; (s/def ::matrix-row (s/coll-of ::matrix-entry :kind vector?))
; (s/def ::matrix (s/and (s/coll-of ::matrix-row
                                  ; :kind vector?)
                       ; (fn [M]
                         ; (let [n (count (first M))]
                           ; (every? #(= n (count %)) M)))))

(defn swap-rows
  "Swap rows a and b of matrix M"
  [M a b]
  (into [] (for [r (range (count M))]
             (cond (= r a) (M b)
                   (= r b) (M a)
                   :else (M r)))))

(defn scale-row
  "Divide all values of row r in matrix M by s"
  [M r s]
  (into [] (for [row (range (count M))]
             (cond (= row r) (mapv #(/ % s) (nth M row))
                   :else (nth M row)))))

(defn subtract-from-all-rows
  "Subtract n times row r in M from all other rows rr, where n is the value of
  the lead column of row rr in M2"
  [M row M2 lead]
  (into []
        (for [r (range (count M))]
          (if (= r row)
            (M r)
            (let [rr (M r)
                  n ((M2 r) lead)
                  new-row-val (fn [val-in-row val-in-rr]
                                (- val-in-rr (* n val-in-row)))]
              (mapv new-row-val (M row) rr))))))

(defn identity-matrix [size]
  (into [] (for [n (range size)]
             (into [] (for [nc (range size)]
                        (if (= n nc) 1 0))))))

(defn first-nonzero-row
  "Find index of first row (starting from r) with nonzero entry in lead column"
  [M r lead]
  (loop [row r]
    (cond (not= 0 ((M row) lead)) row
          (= row (dec (count M))) nil
          :else (recur (inc row)))))

(defn rref
  "Given matrix Mat, reduce it to row echelon form, and return the matrix
  along with the identity matrix of the same column size with the same
  operations applied to it"
  [Mat]
  (let [rows (count Mat)
        cols (count (first Mat))]
    (loop [r 0 lead 0 M Mat I (identity-matrix rows)]
      (if (or (>= r rows) (>= lead cols))
        [M I]
        (let [i (first-nonzero-row M r lead)]
          (cond (nil? i) (recur r (inc lead) M I)
                (not= i r) (recur r lead (swap-rows M r i) (swap-rows I r i))
                :else ;; No swapping, scale and subtract
                (let [k ((M r) lead)]
                  (if-not (= k 1)
                    (recur r lead
                           (scale-row M r k)
                           (scale-row I r k))
                    (recur (inc r) (inc lead)
                           (subtract-from-all-rows M r M lead)
                           (subtract-from-all-rows I r M lead))))))))))

(defn fundamental-dims
  [data] (set (mapcat (comp keys second) (vals data))))

(defn construct-matrix
  [data]
  (let [fundamental (fundamental-dims data)
        n (count fundamental)]
    (when-not (some keyword? fundamental)
      (let [positions (zipmap (range) fundamental)
            val-in-pos (fn [dim pos]
                         (let [dim-name (positions pos)]
                           (get dim dim-name 0)))
            row-vec (fn [dim]
                      (mapv #(val-in-pos (second dim) %) (range n)))]
        (mapv row-vec (vals data))))))

(defn dimensionless-params
  [data]
  (when-let [matrix (construct-matrix data)]
    (loop [[M I] (rref matrix) result []]
      (if (nil? M)
        result
        (let [rowm (first M) rowi (first I)]
          (if-not (every? zero? rowm)
            (recur [(next M) (next I)] result)
            (let [get-den (fn [q] (if (ratio? q) (denominator q) 1))
                  max-den (apply max (map  get-den rowi))
                  is-neg? (if (> (count (filter neg? rowi))
                                 (count (filter pos? rowi)))
                            -1 1)
                  multiplier (* max-den is-neg?)]
              (recur [(next M) (next I)]
                     (conj result
                           (create-vec (map vector (keys data) (map #(* % multiplier) rowi))))))))))))

(defn dimensional-analysis
  [data]
  (when-not (some nil? (vals data))
    (when-let [params (dimensionless-params data)]
      (apply vector
             "Can form the following dimensionless quantities from parameters:"
             (map #(str "\t" (pp-dim % {})) params)))))

(defn err-msg
  [msg & args]
  (fn [data]
    (apply format msg (map (comp pp-type data) args))))

(defn handle-unknown-var
  [data]
  (string/join
   \newline
   [(format "Unknown variable name: %s" (:name data))]))

(defn handle-rtn-type
  [data]
  (string/join
   \newline
   (concat
    ["Failed to match declared type:"
     (format "\t%s" (pp-type (:declared data)))
     "with inferred type:"
     (format "\t%s" (pp-type (:inferred data)))]
    (dimensional-analysis (merge (:args data)
                                 {(:name data) (:declared data)})))))

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
   "match-rtn" handle-rtn-type
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
