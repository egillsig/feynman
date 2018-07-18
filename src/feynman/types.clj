(ns feynman.types
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [feynman.dimensions :as dim]))

(def base-type? keyword?)
(defn compound-type? [t] (and (vector? t)
                              (not= :dimension (first t))
                              (not= :forall (first t))))
(def variable? symbol?)
(defn dim-type? [t] (and (vector? t)
                         (= :dimension (first t))))
(defn type-scheme? [t] (and (vector? t)
                            (= :forall (first t))))
(def type-env? map?)

(defn free-vars
  [t]
  (cond
    (base-type? t) #{}
    (variable? t) #{t}
    (dim-type? t) (set (keys (dim/dim-variables (second t))))
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
                      [:dimension (dim/apply-subs subst dimvec)]))
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
    (dim-type? arg) [:dimension (dim/apply-subs subst (second arg))]
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
