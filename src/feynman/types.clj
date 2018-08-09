;;;; Representation of types

;;; This namespace contains everything related to representation and
;;; manipulation of type information. Monomorphic types can be:
;;; * base type, represented as keywords
;;; * variables,        "       symbols
;;; * Compound types/type applications, notably function and product types,
;;;   represented as a vector starting with name, for example: [:function 'a :Boolean]
;;; * Dimensions/units a special type of compound type with dimension vector
;;;   in place of type argument: [:dimension {"L" 1 "T" -2}]

;;; Type schemes are also represented as vectors of the form:
;;; [:forall #{free-vars} type]

;;; Type environments (contexts) are maps from names to types or type schemes

(ns feynman.types
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.test.check.generators :as gen]
            [feynman.dimensions :as dim]))

(def base-type? keyword?)
(s/def ::base keyword?)

(def variable? symbol?)
(s/def ::variable symbol?)

(defn compound-type? [t] (and (vector? t)
                              (not= :dimension (first t))
                              (not= :forall (first t))))
(s/def ::compound (s/and  (s/cat :kind keyword? :args (s/* ::type))
                          #(not= :dimension (first %))
                          #(not= :forall (first %))))

(defn dim-type? [t] (and (vector? t) (= :dimension (first t))))
(s/def ::dimension (s/with-gen
                     (s/and vector? #(= :dimension (first %)))
                     #(gen/fmap (fn [d] [:dimension d]) (s/gen ::dim/vector))))

(s/def ::type (s/or :base ::base
                    :compound ::compound
                    :variable ::variable
                    :dimension ::dimension))

(defn type-scheme? [t] (and (vector? t) (= :forall (first t))))
(s/def ::scheme (s/cat :kw #{:forall}
                       :vars (s/coll-of ::variable :kind set?)
                       :expr ::type))
(def type-env? map?)
(s/def ::env (s/map-of string? (s/or :type ::type :scheme ::scheme)))

(defn free-vars
  [t]
  (cond
    (base-type? t) #{}
    (variable? t) #{t}
    (dim-type? t) (set (keys (dim/variables (second t))))
    (type-scheme? t) (let [[_ vars exp] t]
                       (set/difference (free-vars exp)
                                       (set vars)))
    (compound-type? t) (let [[_ & r] t]
                         (reduce set/union #{} (map free-vars r)))
    (type-env? t) (reduce set/union #{} (map free-vars (vals t)))))

(defn replace-variable
  "Replace a with b in type t"
  [t a b]
  (cond
    (base-type? t) t
    (variable? t) (if (= t a)
                    (if (string/starts-with? (name b) "d__")
                      ;; Replacing type-var with dim-var, create concrete type
                      [:dimension {b 1}]
                      b)
                    t)
    (dim-type? t) (if (string/starts-with? (name a) "alpha__")
                    ;; No way type variable can be in dimension vector
                    t
                    (let [dimvec (second t)
                          subst {a {b 1}}]
                      [:dimension (dim/apply-subst subst dimvec)]))
    (compound-type? t) (apply vector (map #(replace-variable % a b) t))))

(defn generalize
  [t env]
  (let [free (set/difference (free-vars t) (free-vars env))]
    (if (seq free)
      [:forall free t]
      t)))

(defn new-similar-var
  [v] (if (string/starts-with? (name v) "d__")
        (gensym "d__")
        (gensym "alpha__")))

(defn instantiate
  [v]
  (if (type-scheme? v)
    (let [[_ vars expr] v]
      (reduce (fn [e v]
                (replace-variable e v (new-similar-var v)))
              expr
              vars))
    v))

(defn apply-substitution
  [subst arg]
  (cond
    (base-type? arg) arg
    (variable? arg) (if (contains? subst arg)
                      (apply-substitution subst (subst arg))
                      arg)
    (dim-type? arg) [:dimension (dim/apply-subst subst (second arg))]
    (type-scheme? arg) (let [[_ free expr] arg]
                         [:forall free
                          (apply-substitution (apply dissoc subst free) expr)])
    (compound-type? arg) (apply vector (map (partial apply-substitution subst) arg))
    (type-env? arg) (reduce-kv #(assoc %1 %2 (apply-substitution subst %3)) {} arg)))

(defn unify
  [t1 t2]
  (cond
    (and (base-type? t1) (base-type? t2)) (when (= t1 t2) {})
    (and (variable? t1) (variable? t2)) (if (= t1 t2) {} {t1 t2})
    (variable? t1) (when-not (contains? (free-vars t2) t1) {t1 t2})
    (variable? t2) (when-not (contains? (free-vars t1) t2) {t2 t1})
    (and (dim-type? t1) (dim-type? t2)) (dim/unify (second t1)
                                                   (second t2))
    (and (compound-type? t1) (compound-type? t2))
    (reduce (fn [s [subtype1 subtype2]]
              (when s
                (when-let [new-s (unify
                                  (apply-substitution s subtype1)
                                  (apply-substitution s subtype2))]
                  (merge s new-s))))
            {}
            (map vector t1 t2))))
