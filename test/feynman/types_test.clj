(ns feynman.types-test
  (:require [clojure.test :refer :all]
            [feynman.env :as env]
            [feynman.types :refer :all]))

(deftest helpers

  (testing "free-vars"
    (is (= (free-vars [:function 'a 'b]) #{'a 'b}))
    (is (= (free-vars [:dimension {'d 1}]) #{'d}))
    (is (= (free-vars [:product :type [:dimension {}] 'a]) #{'a}))
    (is (= (free-vars [:product [:dimension {'d 1}] 'a]) #{'d 'a})))

  (testing "replace-variable"
    (is (= (replace-variable 'a 'a 'b) 'b))

    ;; Note that replace-variable always returns legal type expression
    ;; (:dim-variable must be wrapped in :dimension in return value, not in arg)
    (is (= (replace-variable 'a 'a 'd__1)
           [:dimension {'d__1 1}]))

    (is (= (replace-variable [:function 'a 'b] 'b 'd__1)
           [:function 'a [:dimension {'d__1 1}]]))
    (is (= (replace-variable [:product 'a [:dimension {'d__1 1}]]
                             'a
                             'd__1)
           [:product
            [:dimension {'d__1 1}]
            [:dimension {'d__1 1}]]))
    (is (= (replace-variable [:dimension {'d__1 3 'd__2 4 "L" 2}]
                             'd__1
                             'd__2)
           [:dimension {'d__2 7 "L" 2}]))))

(deftest infer-helpers

  (testing "Generalize"
    (is (= (generalize 'a {})
           [:forall #{'a} 'a]))

    (is (= (generalize [:function 'a 'b] {})
           [:forall #{'a 'b}
            [:function 'a 'b]]))

    (is (= (generalize [:function [:dimension {"L" 1 'd 2}] 'a] {})
           [:forall #{'d 'a}
            [:function [:dimension {"L" 1 'd 2}] 'a]])))

  (testing "instantiate"
    (is (variable? (instantiate [:forall #{'a} 'a])))

    (is (dim-type? (instantiate
                    [:forall #{'d__1} [:dimension {'d__1 2}]])))

    (is (compound-type? (instantiate [:forall #{'a 'b} [:function 'a 'b]])))

    (is (compound-type? (instantiate
                         [:forall #{'d1 'd2}
                          [:function [:product [:dimension {'d1 1}] [:dimension {'d2 1}]]
                           [:dimension {'d1 1 'd2 1}]]])))))

(deftest substitutions

  (testing "apply-substitution"
    (let [s {'d__1 {"L" 2} ; Note dim-variables map to vectors, not dims
             'a :type
             'b 'a}] ; (not sure if this is a good idea)

      (is (= (apply-substitution s 'a) :type))

      (is (= (apply-substitution s [:dimension {'d__1 2}])
             [:dimension {"L" 4}]))

      (is (= (apply-substitution s [:function
                                    'a
                                    [:dimension {'d__1 2}]])
             [:function :type [:dimension {"L" 4}]]))

      (is (= (apply-substitution s [:product 'a 'b])
             [:product :type :type]))

      (is (= (apply-substitution s 'c) 'c))))

  (testing "apply-substitution"
    (let [s {'d__1 {"M" 3}
             'a [:function :A :B]}
          a {"foo" [:dimension {'d__1 1}]
             "bar" 'a}
          product {"*" [:forall ['d__1 'd__2]
                        [:function [:product
                                    [:dimension {'d__1 1}]
                                    [:dimension {'d__2 1}]]
                         [:dimension {'d__1 1 'd__2 1}]]]}]

      (is (= (apply-substitution s a)
             {"foo" [:dimension {"M" 3}] "bar" [:function :A :B]}))

      (is (= (apply-substitution s {}) {}))

      (is (= (apply-substitution {} a) a))

      (is (= (apply-substitution
              {}
              product)
             product)))))

(deftest unification

  (testing "unify"
    (is (= (unify :type :type) {}))

    (is (= (unify 'a 'a) {}))

    (is (= (unify 'a 'b) {'a 'b}))

    (is (= (unify 'a [:dimension {'d__1 1}])
           {'a [:dimension {'d__1 1}]}))

    (is (= (unify
            [:product [:dimension {'d__1 1}] [:dimension {'d__2 1}]]
            [:product 'a 'a])
           {'a [:dimension {'d__1 1}]
            'd__1 {'d__2 1}}))

    ;; d_1 x d_2 -> d_1*d_2
    ;; a_1 x a_1 -> a_2
    (is (let [s (unify [:function [:product
                                   [:dimension {'d__1 1}]
                                   [:dimension {'d__2 1}]]
                        [:dimension {'d__1 1 'd__2 1}]]
                       [:function [:product 'a 'a] 'b])]
          (= (apply-substitution s 'b)
             [:dimension {'d__2 2}])))

    (is (= nil
           (unify [:product [:dimension {"L" 1}] [:dimension {"T" 1}]]
                  [:product [:dimension {'d__1 1}] [:dimension {'d__1 1}]])))
    (is (= nil
           (unify [:function [:product
                              [:dimension {"L" 1}]
                              [:dimension {"T" 1}]]
                   'a]
                  [:function [:product
                              [:dimension {'d__1 1}]
                              [:dimension {'d__1 1}]]
                   [:dimension {'d__1 1}]])))))
