(ns feynman.types
  (:require [clojure.core.match :refer [match]]
            [clojure.spec.alpha :as s]
            [feynman.dimensions :as dim]
            [feynman.state :as state]))

;; Concrete types
(s/def ::bool (s/tuple :bool-kw #{:bool}))
(s/def ::dimension (s/tuple :dim-kw #{:dimension} ::dim/dim-vector))

;; Concrete higher-order types
(s/def ::function (s/tuple #{:function} ::mono-type ::mono-type))
(s/def ::product (s/cat :product-kw #{:product} :types (s/+ ::mono-type)))

;; Type expressions
(s/def ::type-variable (s/tuple #{:type-variable} (s/and int? pos?)))

(s/def ::mono-type (s/or :product ::product
                         :function ::function
                         :variable ::type-variable
                         :bool ::bool
                         :dimension ::dimension))

(s/def ::poly-type (s/cat :forall-kw #{:forall}
                          :free-vars (s/coll-of
                                      (s/or :type-variable ::type-variable
                                            :dim-variable ::dim/dim-variable))
                          :mono-type ::mono-type))

(s/def ::type (s/or :mono ::mono-type :poly ::poly-type))

;;; Basic helper fns

(defn type-contains?
  "Does type t contain type variable type-var?"
  [t type-var]
  (match [t]
    [[:bool]] false
    [[:dimension _]] false
    [[:type-variable _]] (= t type-var)
    [[:function t1 t2]] (or (type-contains? t1 type-var) (type-contains? t2 type-var))
    [([:product & r] :seq)] (some #(type-contains? % type-var) r)
    :else false))

(defn free-variables
  [t]
  (match [t]
    [[:bool]] []
    [[:dimension d]] (keys (dim/dim-variables d))
    [[:type-variable _]] [t]
    [[:function t1 t2]] (concat (free-variables t1) (free-variables t2))
    [([:product & r] :seq)] (mapcat free-variables r)
    :else []))

(defn replace-variable
  "Replace var-to-replace with new-var in expr"
  [expr var-to-replace new-var]
  (if (= var-to-replace new-var) ;; This can happen when refreshing variable
    expr
    (match [expr]
      [[:bool]] expr
      [[:type-variable _]] (if (= expr var-to-replace)
                             (if (= :dim-variable (first new-var))
                               [:dimension {new-var 1}]
                               new-var)
                             expr)
      [[:function a b]] [:function
                         (replace-variable a var-to-replace new-var)
                         (replace-variable b var-to-replace new-var)]
      [([:product & r] :seq)] (apply
                               vector
                               :product
                               (map #(replace-variable % var-to-replace new-var) r))
      [[:dimension d]] (if (= :type-variable (first var-to-replace))
                         expr
                         (let [s {var-to-replace {new-var 1}}]
                           [:dimension (dim/apply-subs s d)])))))

(defn make-polymorphic
  "Close type expression t over the assignment A"
  ;; According to paper, the quantified variables should be
  ;; "free dim/type variables in t not in S(A)" (let expressions)
  ;; This is probably not quite correct (keys of a are names, not dim/type variables)
  ;; but I'm not sure what the actual bug is TODO let-generalization
  [t a] [:forall (remove (-> a keys set) (free-variables t)) t])

(defn new-similar-var
  [v s]
  (if (= :dim-variable (first v))
    (state/new-dimension-variable! s)
    (state/new-type-variable! s)))

(defn refresh-variables
  "Replace all free variables in type scheme v with fresh variables"
  [v state]
  (match [v]
    [[:forall vars expr]]
    (reduce (fn [e v]
              (replace-variable e v (new-similar-var v state)))
            expr
            vars)
    :else v))

;;; Substitutions

;; Substitutions can also map dimension variables to dimension vectors, but
;; that case is handled separately in dimensions.clj
;; Again, this cannot be circular, need special generator
(s/def ::substitution (s/map-of ::type-variable ::mono-type))

(declare apply-substitution)

(s/fdef apply-substitution
        :args (s/cat :subs ::substitution
                     :expr (s/or :type ::type
                                 :assignment (s/map-of string? ::type)))
        :ret ::type)

(defn apply-substitution-type-expr
  [s arg]
  (match [arg]
    [[:dimension d]] [:dimension (dim/apply-subs s d)]
    [[:bool]] arg
    [[:type-variable _]] (if (contains? s arg)
                           (apply-substitution s (s arg))
                           arg)
    [[:function t1 t2]] [:function
                         (apply-substitution s t1)
                         (apply-substitution s t2)]
    [([:product & r] :seq)] (apply vector
                                   :product
                                   (map (partial apply-substitution s) r))
    [[:forall free-vars expr]] [:forall free-vars
                                (apply-substitution (apply dissoc s free-vars) expr)]))

(s/fdef apply-substitution-type-expr
        :args (s/cat :subs ::substitution :expr ::type)
        :ret ::type)

(defn apply-substitution-assignment
  [s assignment]
  (reduce-kv #(assoc %1 %2 (apply-substitution-type-expr s %3)) {} assignment))

(defn apply-substitution
  [s arg]
  (if (map? arg)
    (apply-substitution-assignment s arg)
    (apply-substitution-type-expr s arg)))

;;; Unification
(declare unify)

(defn unify-products
  [r1 r2 state]
  (reduce (fn [s [t1 t2]]
            (when-let [ss (unify
                           (apply-substitution s t1)
                           (apply-substitution s t2)
                           state)]
              (merge s ss)))
          {}
          (map vector r1 r2)))

(defn unify
  [t1 t2 state]
  (match [t1 t2]
    [[:bool] [:bool]] {}
    [[:type-variable a] [:type-variable b]] (if (= a b) {} {t1 t2})
    [[:type-variable _] _] (when-not (type-contains? t2 t1) {t1 t2})
    [_ [:type-variable _]] (when-not (type-contains? t1 t2) {t2 t1})
    [[:function tau-1 tau-2]
     [:function tau-3 tau-4]] (let [s1 (unify tau-1 tau-3 state)
                                    s2 (unify tau-2 tau-4 state)]
                                (when (and s1 s2)
                                  (merge s1 s2)))
    [[:dimension d1]  [:dimension d2]] (dim/unify d1 d2 state)
    [[:product & r-1]
     [:product & r-2]] (unify-products r-1 r-2 state)))

(s/fdef unify
        :args (s/cat :type1 ::mono-type :type2 ::mono-type)
        :ret (s/or :error nil?
                   :success ::substitution))
