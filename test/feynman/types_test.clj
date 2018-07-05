(ns feynman.types-test
  (:require [clojure.test :refer :all]
            [feynman.types :refer :all]
            [feynman.state :as state]))

(deftest helpers
  (testing "type-contains?"
    (is (type-contains? [:type-variable 1] [:type-variable 1]))
    (is (not (type-contains? [:type-variable 1] [:type-variable 2])))
    (is (type-contains? [:function [:type-variable 1] [:type-variable 2]]
                        [:type-variable 1]))
    (is (type-contains? [:product [:type-variable 1] [:bool] [:type-variable 2]]
                        [:type-variable 2])))

  (testing "free-variables"
    (is (= (free-variables [:function [:type-variable 1] [:type-variable 2]])
           [[:type-variable 1] [:type-variable 2]]))
    (is (= (free-variables [:dimension {[:dim-variable 1] 1}])
           [[:dim-variable 1]]))
    (is (= (free-variables [:product [:bool] [:dimension {}] [:type-variable 1]])
           [[:type-variable 1]]))
    (is (= (free-variables [:product
                            [:dimension {[:dim-variable 1] 1}]
                            [:type-variable 1]])
           [[:dim-variable 1] [:type-variable 1]])))

  (testing "replace-variable"
    (is (= (replace-variable [:type-variable 1] [:type-variable 1] [:type-variable 2])
           [:type-variable 2]))

    ;; Note that replace-variable always returns legal type expression
    ;; (:dim-variable must be wraped in :dimension in return value, not in arg)
    (is (= (replace-variable [:type-variable 1] [:type-variable 1] [:dim-variable 1])
           [:dimension {[:dim-variable 1] 1}]))

    (is (= (replace-variable [:function [:type-variable 1] [:type-variable 2]]
                             [:type-variable 2]
                             [:dim-variable 1])
           [:function [:type-variable 1] [:dimension {[:dim-variable 1] 1}]]))
    (is (= (replace-variable [:product
                              [:type-variable 1]
                              [:dimension {[:dim-variable 1] 1}]]
                             [:type-variable 1]
                             [:dim-variable 1])
           [:product
            [:dimension {[:dim-variable 1] 1}]
            [:dimension {[:dim-variable 1] 1}]]))
    (is (= (replace-variable [:dimension
                              {[:dim-variable 1] 3
                               [:dim-variable 2] 4
                               "L" 2}]
                             [:dim-variable 1]
                             [:dim-variable 2])
           [:dimension {[:dim-variable 2] 7 "L" 2}]))))

(deftest infer-helpers

  (testing "make-polymorphic"
    (is (= (make-polymorphic [:type-variable 1] {})
           [:forall [[:type-variable 1]] [:type-variable 1]]))

    (is (= (make-polymorphic [:function [:type-variable 1] [:type-variable 2]] {})
           [:forall [[:type-variable 1] [:type-variable 2]]
            [:function [:type-variable 1] [:type-variable 2]]]))

    (is (= (make-polymorphic [:function
                              [:dimension {"L" 1 [:dim-variable 1] 2}]
                              [:type-variable 1]] {})
           [:forall [[:dim-variable 1] [:type-variable 1]]
            [:function
             [:dimension {"L" 1 [:dim-variable 1] 2}]
             [:type-variable 1]]])))

  (testing "refresh-variables"
    (let [state #(state/create-state :dims 2 :types 2)]
      (is (= (refresh-variables
              [:forall [[:type-variable 1]] [:type-variable 1]]
              (state))
             [:type-variable 3]))

      (is (= (refresh-variables
              [:forall [[:dim-variable 1]] [:dimension {[:dim-variable 1] 2}]]
              (state))
             [:dimension {[:dim-variable 3] 2}]))
      (is (= (refresh-variables
              [:forall [[:type-variable 1] [:type-variable 2]]
               [:function [:type-variable 1] [:type-variable 2]]] (state))
             [:function [:type-variable 3] [:type-variable 4]])))))

(deftest substitutions

  (testing "apply-substitution-type-expr"
    (let [s {[:dim-variable 1] {"L" 2} ; Note dim-variables map to vectors, not dims
             [:type-variable 1] [:bool]
             [:type-variable 2] [:type-variable 1]}] ; (not sure if this is a good idea)

      (is (= (apply-substitution-type-expr s [:type-variable 1])
             [:bool]))

      (is (= (apply-substitution-type-expr s [:dimension {[:dim-variable 1] 2}])
             [:dimension {"L" 4}]))

      (is (= (apply-substitution-type-expr s [:function
                                              [:type-variable 1]
                                              [:dimension {[:dim-variable 1] 2}]])
             [:function [:bool] [:dimension {"L" 4}]]))

      (is (= (apply-substitution-type-expr
              s
              [:product [:type-variable 1] [:type-variable 2]])
             [:product [:bool] [:bool]]))

      (is (= (apply-substitution-type-expr s [:type-variable 3])
             [:type-variable 3]))))

  (testing "apply-substitution-assignment"
    (let [s {[:dim-variable 1] {"M" 3}
             [:type-variable 1] [:function [:bool] [:bool]]}
          a {"foo" [:dimension {[:dim-variable 1] 1}]
             "bar" [:type-variable 1]}]

      (is (= (apply-substitution-assignment s a)
             {"foo" [:dimension {"M" 3}] "bar" [:function [:bool] [:bool]]}))

      (is (= (apply-substitution-assignment s {}) {}))

      (is (= (apply-substitution-assignment {} a) a)))))

(deftest unification
  (let [new-state #(state/create-state)]

    (testing "unify"
      (is (= (unify [:bool] [:bool] (new-state)) {}))

      (is (= (unify [:type-variable 1] [:type-variable 1] (new-state)) {}))

      (is (= (unify [:type-variable 1] [:type-variable 2] (new-state))
             {[:type-variable 1] [:type-variable 2]})))))
