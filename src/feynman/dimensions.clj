;;;; Dimension vector operations

;;; All dimensions/units are represented as maps from dimension/unit names
;;; to integers representing the exponent of the corresponding dimension/unit
;;; in the compound expression. Note that this means that vector addition
;;; corresponds to multiplication of expressions, and scalar multiplication
;;; corresponds to exponentiation.

;;; For example, acceleration (L / T^2) would be: {"L" 1 "T" -2}
;;; and time (T) would be: {"T" 1} and the addition of those two "vectors"
;;; results in {"L" 1 "T" -1}, or L/T.


(ns feynman.dimensions
  "Operations relating to dimension vectors"
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [feynman.state :as state]))

;;; Basic specs/operations

;; Dimension variables have the form [:dim-variable <number>]
(s/def ::dim-variable (s/tuple #{:dim-variable} (s/and int? pos?)))

;; Dimension names are either variables or fundamental dimensions
(s/def ::dim-name
  (s/or :fundamental (s/with-gen string? gen/string-ascii)
        :variable ::dim-variable))

;; Dimension vectors are maps from names to nonzero integers
(s/def ::dim-vector (s/map-of ::dim-name (s/and integer? (complement zero?))))

(defn create-dimvec
  "Remove zeros from sequence of key/value pairs and create map"
  [d]
  (into {} (remove #(zero? (second %)) d)))

(defn vmap
  "Map the function f over the values of the dimension vector"
  [f d]
  (create-dimvec (for [[k v] d] [k (f v)])))

(s/fdef vmap
        :args (s/cat :d ::dim-vector :f (s/fspec :args (s/cat :n int?) :ret int?))
        :ret ::dim-vector
        :fn #(every? (-> % :args :d keys set) (-> % :ret keys)))

(defn filter-keys
  [f d] (create-dimvec (for [[k v] d :when (f k)] [k v])))

(defn remove-keys
  [f d] (filter-keys (complement f) d))

(defn split-by
  "Split the dimension vector in two according to the predicate"
  [pred d]
  [(filter-keys pred d) (remove-keys pred d)])

(s/fdef split-by
        :args (s/cat :d ::dim-vector :f (s/fspec :args (s/cat :n int?)))
        :ret (s/cat ::true-keys ::dim-vector ::false-keys ::dim-vector))

#_(defn is-variable? [k] (= (first k) :dim-variable))

(def is-variable? symbol?)
(defn new-dimension-variable [] (gensym "d__"))

(defn dim-variables [d] (filter-keys is-variable? d))

(defn min-exponent-var
  "Return the dimension variable and exponent with smallest absolute value of exponent"
  [d]
  (let [min-dim (->> d
                     dim-variables
                     (vmap #(. Math abs %))
                     (apply min-key second)
                     first)]
    [min-dim (d min-dim)]))

(s/fdef min-exponent-var
        :args (s/cat :d ::dim-vector)
        :ret (s/tuple ::dim-variable (s/and integer? (complement zero?)))
        :fn #(contains? (-> % :args :d)
                        (-> % :ret first)))

;;; Arithmetic operations on dimension vectors

(defn add
  "Add one or more dimension vectors, corresponds to multiplication of the expressions"
  [& args]
  (create-dimvec (apply merge-with + args)))

(defn mul
  "Multiply dimension vector by scalar, corresponding to exponentiation of expression.
  Note that the scalar can be an arbitrary number, the resulting values will be floor-ed"
  [dim scalar]
  (vmap #(int (Math/floor (*' scalar %))) dim))

(defn inv [d] (mul d -1))

(defn divides?
  "Returns true if all exponents in the dimension vector are divisible by n"
  [dim n]
  (every? #(= 0 (mod % n)) (vals dim)))

(s/fdef divides?
        :args (s/cat :dim ::dim-vector :n (s/and int? pos?))
        :ret boolean?)

;;; Substitutions

;; Substitutions map dim-variable names to dim-vectors, and substitutions can
;; be applied to any dim-expressions

(defn apply-subs
  "Apply substitution to a dim-vector.
  A substitution can only map dim-variable names to dim-vectors"
  [s d]
  ;; * Find all dimension variables both in the vector and the substitution
  ;; * Scale the values in the substitution by the corresponding exponent
  ;; * Add all the scaled vectors to the original one without the variables
  (let [[to-subs unchanged] (split-by (set (keys s)) d)
        multiplied (map #(mul (s %) (d %)) (keys to-subs))]
    (if (empty? to-subs)
      d
      (recur s (add unchanged (apply add multiplied))))))

;; Substitutions are very hard to spec because they are not allowed to be
;; circular, i.e. d1 -> d2, d2 -> d3, d3 -> d1
;; TODO: create custom generator to avoid this
(s/def ::dim-subs
  (s/and
   (s/map-of ::dim-variable ::dim-vector)
   (fn [s] (every? #(not (contains? (second %) (first %))) s))))

(s/fdef apply-subs
        :args (s/cat :subs ::dim-subs
                     :d ::dim-vector)
        :ret ::dim-vector)

;;; Dimensional unification

(defn unify-with-one
  "Find substitution to that unifies dim with a dimensionless quantity"
  [expr]
  (loop [s {} d expr]
    (cond (empty? d) {}
          (empty? (dim-variables d)) nil
          :else
          (let [[d-1 x-1] (min-exponent-var d)
                divided (-> d (mul (/ 1 x-1)) inv (dissoc d-1))]
            (if (divides? d x-1)
              (assoc s d-1 divided)
              (let [newdim (new-dimension-variable)
                    subval (add divided {newdim 1})
                    new-s (assoc s d-1 subval)
                    new-d (apply-subs new-s d)]
                (if (= 1 (count (dim-variables new-d)))
                  nil
                  (recur new-s new-d))))))))

(s/fdef unify-with-one
        :args (s/cat :expr ::dim-vector :state any?)
        :ret (s/or :nil nil? :substitution ::dim-subs)
        :fn #(= {}
                (apply-subs (-> % :ret)
                            (-> % :args :expr))))

(defn unify
  "Find substitution to that unifies the two dimension expressions

  Note that:
    d1 = d2
  Implies:
    d1 + (-d2) = 0 (In vector notaion)
  Or equivalently:
    d1 * (1/d2) = 1 (For the dimension expressions)"
  [d1 d2] (unify-with-one (add d1 (inv d2))))
