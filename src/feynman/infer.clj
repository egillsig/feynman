(ns feynman.infer
  (:require [clojure.core.match :refer [match]]
            [feynman.dimensions :as dim]
            [feynman.types :as t]
            [puget.printer :refer [pprint]]))

(defn transpose
  [coll]
  (apply (partial map vector) coll))

(defn new-type-var ([] (gensym "alpha__"))
  ([prefix] (gensym (str "alpha_" prefix "__"))))

(defn eval-exponent [e]
  (match [e]
    [[:integer i]] i
    [[:unary-minus-exponent i]] (- (eval-exponent i))
    [[:unary-plus-exponent i]] (eval-exponent i)))

;; Denotable type/unit expressions
;; TODO:
;; * What ns does this belong to
;; * Add error handling
;; * More generic handling of compound types
;; * Think about tree traversal, clojure.walk?
(defn unit-expr-vars
  [expr]
  (match [expr]
    [[(:or :mul-units :div-units) a b]] (merge (unit-expr-vars a)
                                               (unit-expr-vars b))
    [[:exp-unit a b]] (unit-expr-vars a)
    [[:name _]] {}
    [[:dimensionless]] {}
    [[:var-type [:name n]]] {n (dim/new-dimension-variable)}))

(defn type-expr-vars
  [expr]
  (letfn
   [(vars [e acc]
      (match [e]
        [[(:or :func-type :product-type) & r]]
        (reduce #(merge %1 (vars %2 %1)) acc r)
        [[:type-name _]] acc
        [[:var-type [:name n]]] (if (contains? acc n)
                                  acc
                                  (assoc acc n (new-type-var)))
        [v] (merge acc (unit-expr-vars v))))]
    (vars expr {})))

(defn eval-unit-expr
  [env expr]
  (letfn [(evl [e]
            (match [e]
              [[:mul-units a b]] (dim/add (evl a) (evl b))
              [[:div-units a b]] (dim/add (evl a) (dim/inv (evl b)))
              [[:exp-unit a b]]  (dim/mul (evl a) (eval-exponent b))
              [[:name n]] (if (contains? env n)
                            (second (env n))
                            (throw (ex-info "unknown-unit" {:name n})))
              [[:dimensionless]] {}
              [[:var-type [:name n]]] {(env n) 1}))]
    [:dimension (evl expr)]))

;; eval-type-expr needs to accept type env param to recognize defined units
;; Also needs to return some substitution for mapping of type variables
;; that might be used again
(defn eval-type-expr
  [type-env expr]
  (let [env (merge (type-expr-vars expr) type-env)]
    (letfn [(evl [e]
              (match [e]
                [[:func-type a b]] [:function [:product (evl a)] (evl b)]
                [[:product-type & r]] (apply vector :product (map evl r))
                [[:type-name n]] (env n)
                [[:var-type [:name n]]] (env n)
                [v] (eval-unit-expr env v)))]
      [env (evl expr)])))

(defn arg-fn
  "Function to reduce over argument list, requires a env/type tuple
  and an argument, returns env/type tuple"
  [[A t] arg]
  (match [arg]
    [[:arg [:name arg-name]]]
    (let [new-t (new-type-var)]
      [(assoc A arg-name new-t) (conj t new-t)])
    [[:arg [:name arg-name] type-expr]]
    (let [[tenv new-t]
          (eval-type-expr (:types A) type-expr)]
      [(merge-with merge A {arg-name new-t :types tenv}) (conj t new-t)])))

(defn infer-args-list
  [A expr]
  (reduce arg-fn [A [:product]] (rest expr)))

(declare infer)

(defn infer-bool
  [_ _] [{} :Boolean])

(defn infer-number
  [_ expr]
  (if (zero? (second expr))
    [{}
     ;; Should this be instantiated or type scheme?
     (t/instantiate
      [:forall #{'d__1} [:dimension {'d__1 1}]])]
    [{} [:dimension {}]]))

(defn infer-name
  [A expr]
  (if-let [value (get A (second expr))]
    [{} (t/instantiate value)]
    (throw (ex-info "unknown-var"
                    {:name (second expr)
                     :expr expr
                     :env A}))))

(defn merge-or-latter [a b] (if (map? a) (merge a b) b))

(defn infer-function
  [A expr]
  (let [[_ arg-list fn-body] expr
        [args-map args-type] (infer-args-list A arg-list)
        [s tau] (infer (merge-with merge-or-latter A args-map) fn-body)
        functype [:function (t/apply-substitution s args-type) tau]]
    [s functype]))

(defn def-name
  [expr]
  (match [expr]
    [[(:or :func-def :def :unit-def) [:name n] & r]] n))

(defn infer-def
  [A expr]
  (match [expr]
    [[:func-def [:name func-name] arg-list def-expr]]
    (infer-function
     (assoc A func-name (new-type-var))
     [:function arg-list def-expr])
    [[:func-def [:name func-name] arg-list ret-type def-expr]]
    (let [[s inferred-functype] (infer-function
                                 (assoc A func-name (new-type-var)) ;FIXME: the pre-guessed type of func is never unified with inferred type
                                 [:function arg-list def-expr])
          [_ _ inferred-ret-type] inferred-functype
          [_ declared-ret-type] (eval-type-expr A ret-type)]
      (if (not (t/unify inferred-ret-type declared-ret-type))
        (throw (ex-info "match-rtn"
                        {:inferred inferred-ret-type
                         :declared declared-ret-type
                         :expr expr
                         :env A}))
        [s inferred-functype]))

    [[:def [:name var-name] def-expr]] (infer A def-expr)
    [[:def [:name var-name] var-type def-expr]]
    (let [[s inferred-t] (infer A def-expr)
          declared-t (eval-type-expr A var-type)]
      (if (not= inferred-t declared-t)
        (throw (ex-info "match-var"
                        {:inferred inferred-t
                         :declared declared-t
                         :expr expr
                         :env A}))
        [s inferred-t]))
    [[:unit-def [:name unit-name]]]
    [{} [:dimension {unit-name 1}]]
    [[:unit-def [:name unit-name] unit-expr]]
    [{} (eval-unit-expr (:types A) unit-expr)]))

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
        s1 (merge s1 (t/unify tau1 :Boolean))]
    (if-not s1
      (throw (ex-info "if-not-bool" {:cond-type tau1
                                     :expr expr
                                     :env A}))
      (let [[s2 tau2] (infer (t/apply-substitution s1 A) if-if)
            [s3 tau3] (infer (t/apply-substitution (merge s1 s2) A) if-else)
            tau2 (t/apply-substitution s3 tau2)
            s4 (t/unify tau2 tau3)]
        (println "If type: " tau2)
        (println "Else type: " tau3)
        (if s4
          [(merge s1 s2 s3 s4) (t/apply-substitution s4 tau3)]
          (throw (ex-info "match-if-else"
                          {:if-type tau2
                           :else-type tau3
                           :expr expr
                           :env A})))))))

(defn infer-args
  [A args]
  (loop [subst {} t [:product] as args]
    (if (empty? as)
      [subst t]
      (let [arg (first as)
            [s2 t2] (infer (t/apply-substitution subst A) arg)]
        (recur (merge subst s2)
               (conj t t2)
               (rest as))))))

(defn infer-apply
  [A expr]
  (let [[_ func-name & func-args] expr
        [s1 tau1] (infer A func-name)
        [args-subs args-type] (infer-args (t/apply-substitution s1 A) func-args)
        alpha (new-type-var (second func-name))
        s3 (t/unify (t/apply-substitution args-subs tau1)
                    [:function (t/apply-substitution args-subs args-type) alpha])]
    (if s3
      [(merge s1 args-subs s3) (t/apply-substitution s3 alpha)]
      (throw (ex-info "match-func-args"
                      {:func-type tau1
                       :args-type args-type
                       :expr expr
                       :env A})))))

(defn infer-
  [A expr]
  (match [expr]
    [[:boolean _]] (infer-bool A expr)
    [[:name _]] (infer-name A expr)
    [[:number _]] (infer-number A expr)
    [[:function _ _]] (infer-function A expr)
    [[:let _ _]] (infer-let A expr)
    [[:if _ _ _]] (infer-if A expr)
    [[:apply & _]] (infer-apply A expr)
    [[(:or :def :func-def :unit-def) & _]] (infer-def A expr)
    [[_ subexpr]] (infer A subexpr)
    :else nil))

(defn infer
  [A expr]
  ; (println "Inferring" (transpile expr))
  (let [[subst t] (infer- A expr)]
    (pprint subst)
    (println "type: " t)
    (println "for:" expr)
    [subst t]))

(defn infer-type
  [A expr]
  (let [[subst e-type] (infer A expr)
        e-type (t/generalize (t/apply-substitution subst e-type) A)]
    (cond (#{:def :func-def} (first expr))
          [(def-name expr) e-type nil]
          (= :unit-def (first expr))
          [(def-name expr) e-type :unit]
          :else
          [nil e-type nil])))
