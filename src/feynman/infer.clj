(ns feynman.infer
  (:require [clojure.core.match :refer [match]]
            [feynman.types :as t]))

(defn transpose
  [coll]
  (apply (partial map vector) coll))

(defn new-type-var [] (gensym "alpha__"))

(declare infer)

(defn infer-bool
  [_ _] [{} :Boolean])

(defn infer-number
  ;; TODO: zero is polymorphic
  [_ _] [{} [:dimension {}]])

(defn infer-name
  [A expr]
  (if-let [value (get A (second expr))]
    [{} (t/instantiate value)]
    (throw (ex-info "Unknown variable name"
                    {:name (second expr)
                     :expr expr
                     :env A}))))

; Note: infer-args-list does not return substitution but map of names to types
; and product type
(defn infer-args-list
  [A expr]
  (let [args (rest expr)
        arg-type (fn [a]
                   (match [a]
                     [[:arg [:name arg-name]]]
                     (new-type-var)
                     [[:arg [:name arg-name] type-expr]]
                     (second (infer A type-expr))))
        arg-types (map arg-type args)
        arg-name (fn [a] (-> a second second))
        arg-names (map arg-name args)]
    [(zipmap arg-names arg-types) (apply vector :product arg-types)]))

(defn infer-function
  [A expr]
  (let [[_ arg-list fn-body] expr
        [args-map args-type] (infer-args-list A arg-list)
        [s tau] (infer (merge A (into {} args-map)) fn-body)
        functype [:function (t/apply-substitution s args-type) tau]]
    [s functype]))

(defn def-name
  [expr]
  (match [expr]
    [[:func-def [:name func-name] & r]] func-name
    [[:def [:name var-name] & r]] var-name))

(defn infer-def
  [A expr]
  (match [expr]
    [[:func-def [:name func-name] arg-list def-expr]]
    (infer-function
     (assoc A func-name (new-type-var))
     [:function arg-list def-expr])
    [[:func-def [:name func-name] arg-list ret-type def-expr]]
    (let [[s inferred-functype] (infer-function
                                 (assoc A func-name (new-type-var))
                                 [:function arg-list def-expr])
          [_ _ inferred-ret-type] inferred-functype
          declared-ret-type (infer A ret-type)]
      (if (not= inferred-ret-type declared-ret-type)
        (throw (ex-info "Failed to match declared return type with inferred type"
                        {:inferred inferred-ret-type
                         :declared declared-ret-type
                         :expr expr
                         :env A}))
        [s inferred-functype]))

    [[:def [:name var-name] def-expr]] (infer A def-expr)
    [[:def [:nane var-name] var-type def-expr]]
    (let [[s inferred-t] (infer A def-expr)
          declared-t (infer A var-type)]
      (if (not= inferred-t declared-t)
        (throw (ex-info "Failed to match declared type with inferred type"
                        {:inferred inferred-t
                         :declared declared-t
                         :expr expr
                         :env A}))
        [s inferred-t]))))

(defn infer-let
  [A expr]
  (let [[_ let-def let-expr] expr
        let-name (def-name let-def)
        [s1 tau1] (infer-def A let-def)
        a2 (assoc A
                  let-name
                  (t/generalize tau1 (t/apply-substitution s1 A)))
        [s2 tau2] (infer a2 let-expr)]
    [(merge s1 s2) tau2]))

(defn infer-if
  [A expr]
  (let [[_ if-condition if-if if-else] expr
        [s1 tau1] (infer A if-condition)
        s1 (t/unify tau1 :Boolean)]
    (if-not s1
      (throw (ex-info "If-condition not boolean" {:cond-type tau1
                                                  :expr expr
                                                  :env A}))
      (let [[s2 tau2] (infer (t/apply-substitution s1 A) if-if)
            [s3 tau3] (infer (t/apply-substitution s2 A) if-else)
            tau2 (t/apply-substitution s3 tau2)
            s4 (t/unify tau2 tau3)]
        (if s4
          [(merge s1 s2 s3 s4) (t/apply-substitution s4 tau3)]
          (throw (ex-info "Failed to match if/else branches"
                          {:if-type tau2
                           :else-type tau3
                           :expr expr
                           :env A})))))))

(defn infer-apply
  [A expr]
  (let [[_ func-name & func-args] expr
        [s1 tau1] (infer A func-name)
        type-map (map #(infer (t/apply-substitution s1 A) %) func-args)]
    (let [[args-subs args-types] (transpose type-map)
          args-subs (apply merge args-subs)
          args-type (apply vector :product args-types)
          alpha (new-type-var)
          s3 (t/unify (t/apply-substitution args-subs tau1)
                      [:function args-type alpha])]
      (if s3
        [(merge s1 args-subs s3) (t/apply-substitution s3 alpha)]
        (throw (ex-info "Failed to match function type with arguments."
                        {:func-type tau1
                         :args-type args-type
                         :expr expr
                         :env A}))))))

(defn infer
  [A expr]
  (match [expr]
    [[:boolean _]] (infer-bool A expr)
    [[:name _]] (infer-name A expr)
    [[:number _]] (infer-number A expr)
    [[:function _ _]] (infer-function A expr)
    [[:let _ _]] (infer-let A expr)
    [[:if _ _ _]] (infer-if A expr)
    [[:apply & _]] (infer-apply A expr)
    [[:def & _]] (infer-def A expr)
    [[:func-def & _]] (infer-def A expr)
    [[_ subexpr]] (infer A subexpr)
    :else nil))
