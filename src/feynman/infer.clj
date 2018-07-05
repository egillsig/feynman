(ns feynman.infer
  (:require [feynman.types :as t]
            [feynman.state :as state]
            [clojure.core.match :refer [match]]))

(defn transpose
  [coll]
  (apply (partial map vector) coll))

(defn combine-errs
  [& args] args)

(defn make-error
  [& args] args)

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
    [nil {} (t/refresh-variables value state)]
    [(make-error "Unknown variable name" expr) nil nil]))

(defn infer-function
  [A expr state]
  (let [[_ arg-list fn-body] expr
        alphas (into {} (for [arg arg-list] [arg (state/new-type-variable! state)]))
        args-type (apply vector :product (keys alphas))
        [err s tau] (infer (merge A alphas) fn-body state)]
    (if err
      [err nil nil]
      (let [functype [:function (t/apply-substitution s args-type) tau]]
        [nil s functype]))))

(defn infer-let
  [A expr state]
  (let [[let-name let-val let-expr] expr
        [err s1 tau1] (infer A let-val state)]
    (if err
      [err nil nil]
      (let [a2 (assoc A let-name (t/make-polymorphic tau1 (t/apply-substitution s1 A)))
            [err tau2 s2] (infer a2 let-expr state)]
        (if err
          [err nil nil]
          [nil tau2 (merge s1 s2)])))))

(defn infer-if
  [A expr state]
  (let [[_ if-condition if-if if-else] expr
        [err s1 tau1] (infer A if-condition state)]
    (cond
      err [err nil nil]
      (not= [:bool] tau1) [(make-error "If-condition not boolean" expr) nil nil]
      :else (let [[err s2 tau2] (infer (t/apply-substitution s1 A) if-if state)]
              (if err
                [err nil nil]
                (let [[err s3 tau3] (infer (t/apply-substitution s2 A) if-else state)
                      s4 (t/unify (t/apply-substitution s3 tau2) tau3 state)]
                  (if s4
                    [nil (merge s1 s2 s3 s4) (t/apply-substitution s4 tau3)]
                    [(make-error "Failed to match if/else branches" expr) nil nil])))))))

(defn infer-apply
  [A expr state]
  (let [[_ func-name [_ & func-args]] expr
        [err s1 tau1] (infer A func-name state)]
    (if err
      [err nil nil]
      (let [type-map (map #(infer (t/apply-substitution s1 A) % state) func-args)
            [errs args-subs args-types] (transpose type-map)
            errs      (filter some? errs)
            args-subs (apply merge args-subs)
            args-type (apply vector :product args-types)]
        (if (seq errs)
          (combine-errs errs expr)
          (let [alpha (state/new-type-variable! state)
                s3 (t/unify (t/apply-substitution args-subs tau1)
                            [:function args-type alpha]
                            state)]
            (println args-type alpha (t/apply-substitution args-subs tau1))
            (if s3
              [nil (merge s1 args-subs s3) (t/apply-substitution s3 alpha)]
              [(make-error "Type mismatch" expr) nil nil])))))))

(defn infer
  ([A expr] (infer A expr (state/create-state)))
  ([A expr state]
   (match [expr]
     [[:boolean _]] (infer-bool A expr state)
     [[:name _]] (infer-name A expr state)
     [[:number _]] (infer-number A expr state)
     [[:function _ _]] (infer-function A expr state)
     [[:let _ _]] (infer-let A expr state)
     [[:if _ _ _]] (infer-if A expr state)
     [[:apply _ _]] (infer-apply A expr state)
     [[_ subexpr]] (infer A subexpr state)
     :else nil)))
