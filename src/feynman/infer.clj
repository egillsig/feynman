(ns feynman.infer
  (:require [clojure.core.match :refer [match]]
            [feynman.dimensions :as dim]
            [feynman.state :as state]
            [feynman.types :as t]))

(defn transpose
  [coll]
  (apply (partial map vector) coll))

;;TODO: :dimension boxing mess
(defn parse-type-expr [A e]
  (match [e]
    [[_ [:name dim-name]]] (if (contains? A e) (A e) [:dimension {dim-name 1}])
    [[_ e1 "*" e2]] (dim/add (parse-type-expr A e1) (parse-type-expr A e2))
    [[_ e1 "/" e2]] (dim/add (parse-type-expr A e1) (dim/inv (parse-type-expr A e2)))
    [[_ e "^" n]] (dim/mul (parse-type-expr A e) n)))

;; infer : Assign x expr x state -> errors x substitution x type
;; State possibly mutated
(declare infer)

(defn infer-bool
  [A expr state] [nil {} [:bool]])

(defn infer-number
  [A expr state] [nil {} [:dimension {}]])

(defn infer-name
  [A expr state]
  (if-let [value (get A (second expr))]
    [{} (t/refresh-variables value state)]
    (throw (ex-info "Unknown variable name"
                    {:name (second expr)
                     :expr expr
                     :env A}))))

;; TODO: Functions with declared types for arguments
(defn infer-function
  [A expr state]
  (let [[_ [_  & arg-list] fn-body] expr
        alphas (for [arg arg-list] [(second arg) (state/new-type-variable! state)])
        args-type (apply vector :product (map second alphas))
        [s tau] (infer (merge A (into {} alphas)) fn-body state)
        functype [:function (t/apply-substitution s args-type) tau]]
    [s functype]))

(defn def-name
  [expr]
  (match [expr]
    [[:func-def [:name func-name] & r]] func-name
    [[:def [:name var-name] & r]] var-name))

(defn infer-def
  [A expr state]
  (match [expr]
    [[:func-def [:name func-name] arg-list def-expr]]
    (infer-function
     (assoc A func-name (state/new-type-variable!))
     [:function arg-list def-expr]
     state)

    [[:func-def [:name func-name] arg-list ret-type def-expr]]
    (let [[s inferred-functype] (infer-function
                                 (assoc A func-name (state/new-type-variable!))
                                 [:function arg-list def-expr]
                                 state)
          [_ _ inferred-ret-type] inferred-functype
          declared-ret-type (parse-type-expr A ret-type)]
      (if (not= inferred-ret-type declared-ret-type)
        (throw (ex-info "Failed to match declared return type with inferred type"
                        {:inferred inferred-ret-typet
                         :declared declared-ret-type
                         :expr expr
                         :env A}))
        [s inferred-functype]))

    [[:def [:name var-name] def-expr]]
    (infer A def-expr state)

    [[:def [:nane var-name] var-type def-expr]]
    (let [[s inferred-t] (infer A def-expr state)
          declared-t (parse-type-expr A var-type)]
      (if (not= inferred-t declared-t)
        (throw (ex-info "Failed to match declared type with inferred type"
                        {:inferred inferred-t
                         :declared declared-t
                         :expr expr
                         :env A}))
        [s inferred-t]))))

(defn infer-let
  [A expr state]
  (let [[_ let-name let-val let-expr] expr
        [s1 tau1] (infer A let-val state)
        a2 (assoc A
                  (second let-name)
                  (t/make-polymorphic tau1 (t/apply-substitution s1 A)))
        [s2 tau2] (infer a2 let-expr state)]
    [(merge s1 s2) tau2]))

(defn infer-if
  [A expr state]
  (let [[_ if-condition if-if if-else] expr
        [s1 tau1] (infer A if-condition state)]
    (if (not= [:bool] tau1)
      (throw (ex-info "If-condition not boolean" {:cond-type tau1
                                                  :expr expr
                                                  :env A}))
      (let [[s2 tau2] (infer (t/apply-substitution s1 A) if-if state)
            [s3 tau3] (infer (t/apply-substitution s2 A) if-else state)
            tau2 (t/apply-substitution s3 tau2)
            s4 (t/unify tau2 tau3 state)]
        (if s4
          [(merge s1 s2 s3 s4) (t/apply-substitution s4 tau3)]
          (throw (ex-info "Failed to match if/else branches"
                          {:if-type tau2
                           :else-type tau3
                           :expr expr
                           :env A})))))))

(defn infer-apply
  [A expr state]
  (let [[_ func-name [_ & func-args]] expr
        [s1 tau1] (infer A func-name state)]
    (let [type-map (map #(infer (t/apply-substitution s1 A) % state) func-args)
          [args-subs args-types] (transpose type-map)
          args-subs (apply merge args-subs)
          args-type (apply vector :product args-types)
          alpha (state/new-type-variable! state)
          s3 (t/unify (t/apply-substitution args-subs tau1)
                      [:function args-type alpha]
                      state)]
      (if s3
        [(merge s1 args-subs s3) (t/apply-substitution s3 alpha)]
        (throw (ex-info "Failed to match function type with arguments."
                        {:func-type tau1
                         :args-type args-type
                         :expr expr
                         :env A}))))))

(defn infer
  ([A expr] (infer A expr (state/create-state)))
  ([A expr state]
   (match [expr]
     [[:boolean _]] (infer-bool A expr state)
     [[:name _]] (infer-name A expr state)
     [[:number _]] (infer-number A expr state)
     [[:function _ _]] (infer-function A expr state)
     [[:let _ _ _]] (infer-let A expr state)
     [[:if _ _ _]] (infer-if A expr state)
     [[:apply _ _]] (infer-apply A expr state)
     [[_ subexpr]] (infer A subexpr state)
     :else nil)))
