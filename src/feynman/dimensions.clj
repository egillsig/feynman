;;;; Dimension vector operations

;;; All dimensions/units are represented as maps from dimension/unit names
;;; to integers representing the exponent of the corresponding dimension/unit
;;; in the compound expression. Note that this means that vector addition
;;; corresponds to multiplication of expressions, and scalar multiplication
;;; corresponds to exponentiation.

;;; For example, acceleration (L / T^2) would be: {"L" 1 "T" -2}:
;;; and time (T) would be: {"T" 1} and the addition of those two "vectors"
;;; results in {"L" 1 "T" -1}, or L/T.

;;; Keys in a dimension vector can also be variables, represented as symbols
;;; with d__ as prefix, for example the expression d_1 ^ 2 * d_2 * L would be
;;; {'d__1 2 'd__2 1 "L" 1}?

(ns feynman.dimensions
  "Operations relating to dimension vectors"
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as string]
            [clojure.math.numeric-tower :as math]))

;;; Basic specs/operations

(s/def ::variable (s/with-gen
                    (s/and symbol? #(-> % name (string/starts-with? "d__")))
                    #(gen/fmap (fn [d] (symbol (str "d__" d))) (gen/int))))

;; Dimension names are either variables or fundamental dimensions
(s/def ::name
  (s/or :fundamental (s/with-gen string? gen/string-alphanumeric)
        :variable ::variable))

;; Dimension vectors are maps from names to nonzero integers
(s/def ::vector (s/map-of ::name (s/and integer? (complement zero?))))

(defn create-vec
  "Remove zeros from sequence of key/value pairs and create map"
  [d]
  (into {} (remove #(zero? (second %)) d)))

(defn vmap
  "Map the function f over the values of the dimension vector"
  [f d]
  (create-vec (for [[k v] d] [k (f v)])))

(defn filter-keys
  [f d] (create-vec (for [[k v] d :when (f k)] [k v])))

(defn remove-keys
  [f d] (filter-keys (complement f) d))

(defn split-by
  "Split the dimension vector in two according to the predicate"
  [pred d]
  [(filter-keys pred d) (remove-keys pred d)])

(def is-variable? symbol?)

(defn new-dimension-variable [] (gensym "d__"))

(defn variables [d] (filter-keys is-variable? d))

(defn min-exponent-var
  "Return the dimension variable and exponent with smallest absolute value of
  exponent"
  [d]
  (let [vars (variables d)]
    (when (seq vars)
      (let [min-dim (->> vars
                         (vmap math/abs)
                         (apply min-key second)
                         first)]
        [min-dim (d min-dim)]))))

;;; Arithmetic operations on dimension vectors

(defn add
  "Add one or more dimension vectors, corresponds to multiplication of the
  expressions"
  [& args]
  (create-vec (apply merge-with +' args)))

(defn mul
  "Multiply dimension vector by scalar, corresponding to exponentiation of
  expression.  Note that the scalar can be an arbitrary number, the resulting
  values will be floor-ed"
  [dim scalar]
  (vmap #(let [v (math/floor (*' scalar %))]
           (if (or (> v Integer/MAX_VALUE)
                   (< v Integer/MIN_VALUE)) (bigint v) (int v)))
        dim))

(defn inv [d] (mul d -1))

(defn divides?
  "Returns true if all exponents in the dimension vector are divisible by n"
  [dim n]
  (every? #(= 0 (mod % n)) (vals dim)))

;;; Substitutions

;; Substitutions map variable names to vectors, and substitutions can
;; be applied to any expressions

;; Substitutions cannot be cyclic, i.e. d1 -> d2, d2 -> d3, d3 -> d1

(defn cyclic?
  [subst]
  (letfn [(neighbours [node] (keys (variables (subst node))))
          (dfs [node visited]
               (or (contains? visited node)
                   (some #(dfs % (conj visited node)) (neighbours node))))]
    (some #(dfs % #{}) (keys subst))))

(s/def ::subst (s/and (s/map-of ::variable ::vector)
                      (complement cyclic?)))

(defn apply-subst
  "Apply substitution to a dim-vector.
  A substitution can only map variable names to vectors"
  [s d]
  ;; * Find all dimension variables both in the vector and the substitution
  ;; * Scale the values in the substitution by the corresponding exponent
  ;; * Add all the scaled vectors to the original one without the variables
  (let [[to-subst unchanged] (split-by (set (keys s)) d)
        multiplied (map #(mul (s %) (d %)) (keys to-subst))]
    (if (empty? to-subst)
      d
      (recur s (add unchanged (apply add multiplied))))))

(s/fdef apply-subst
        :args (s/cat :subst ::subst
                     :d ::vector)
        :ret ::vector
        ;; Should be idempotent
        :fn #(= (-> % :ret)
                (apply-subst (-> :arg :subst)
                             (-> % :ret))))

;;; Dimensional unification

(defn unify-with-one
  "Find substitution to that unifies dim with a dimensionless quantity"
  [expr]
  (loop [s {} d expr]
    (cond (empty? d) {}
          (empty? (variables d)) nil
          :else
          (let [[d-1 x-1] (min-exponent-var d)
                divided (-> d (mul (/ 1 x-1)) inv (dissoc d-1))]
            (if (divides? d x-1)
              (assoc s d-1 divided)
              (let [newdim (new-dimension-variable)
                    subval (add divided {newdim 1})
                    new-s (assoc s d-1 subval)
                    new-d (apply-subst new-s d)]
                (if (= 1 (count (variables new-d)))
                  nil
                  (recur new-s new-d))))))))

(s/fdef unify-with-one
        :args (s/cat :expr ::vector)
        :ret (s/or :nil nil? :substitution ::subst)
        :fn #(let [ret (-> % :ret second)]
               (or (nil? ret)
                   (= {} (apply-subst ret (-> % :args :expr))))))

(defn unify
  "Find substitution to that unifies the two dimension expressions"
  ;; Note that:
  ;;   d1 = d2
  ;; Implies:
  ;;   d1 + (-d2) = 0 (In vector notaion)
  ;; Or equivalently:
  ;;   d1 * (1/d2) = 1 (For the dimension expressions)
  [d1 d2] (unify-with-one (add d1 (inv d2))))
