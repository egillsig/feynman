(ns feynman.infer-test
  (:require [clojure.test :refer :all]
            [feynman.env :as env]
            [feynman.infer :refer :all]))

(deftest denotable-types
  (testing "eval-exponent"
    (is (= 3 (eval-exponent [:integer 3])))
    (is (= -1 (eval-exponent [:unary-minus-exponent [:integer 1]]))))

  (testing "unit-expr-vars"
    (is (= {} (unit-expr-vars [:name "m"])))
    (is (= {} (unit-expr-vars [:dimensionless])))
    (is (= ["A" "B"] (keys (unit-expr-vars
                            [:mul-units
                             [:var-type [:name "A"]]
                             [:div-units
                              [:name "m"]
                              [:mul-units
                               [:var-type [:name "B"]]
                               [:var-type [:name "A"]]]]]))))

    (testing "type-expr-vars"
      (is (= ["A" "B"] (keys (type-expr-vars
                              [:mul-units
                               [:var-type [:name "A"]]
                               [:div-units
                                [:name "m"]
                                [:mul-units
                                 [:var-type [:name "B"]]
                                 [:var-type [:name "A"]]]]]))))

      (is (= ["A" "B"] (keys (type-expr-vars
                              [:func-type [:var-type [:name "A"]]
                               [:var-type [:name "B"]]])))))
    (testing "eval-unit-expr"
      (is (= [:dimension {"m" 1}]
             (eval-unit-expr {"m" [:dimension {"m" 1}]}
                             [:name "m"])))
      (is (= [:dimension {"m" 2}]
             (eval-unit-expr {"m" [:dimension {"m" 1}]}
                             [:mul-units [:name "m"] [:name "m"]])))
      (is (= [:dimension {'d__1 1}]
             (eval-unit-expr {"A" 'd__1}
                             [:var-type [:name "A"]]))))
    (testing "eval-type-expr"
      (is (= [{"Type" :Type} :Type] (eval-type-expr {"Type" :Type}
                                                    [:type-name "Type"])))
      (is (= [{"A" 'a} 'a] (eval-type-expr {"A" 'a}
                                           [:var-type [:name "A"]])))
      (let [[newenv t] (eval-type-expr {}
                                       [:func-type
                                        [:var-type [:name "A"]]
                                        [:var-type [:name "B"]]])
            a-type (newenv "A")
            b-type (newenv "B")]
        (is (= ["A" "B"] (keys newenv)))
        (is (= t [:function [:product a-type] b-type]))))))

(deftest basic-infer-funcs
  (testing "infer-name"
    (is (= (infer-name {"m" :type} [:name "m"]) [{} :type]))

    (is (infer-name {"square" [:forall #{'d__1}
                               [:function [:dimension {'d__1 1}]
                                [:dimension {'d__1 2}]]]}
                    [:name "square"])))

  (testing "infer-args-list"
    (let [[e t] (infer-args-list {} [:arg-list [:arg [:name "x"]]])
          x-type (e "x")]
      (is (= t [:product x-type])))

    (is (= [{:types {"L" [:dimension {"L" 1}]}
             "x" [:dimension {"L" 1}]}
            [:product [:dimension {"L" 1}]]]
           (infer-args-list {:types {"L" [:dimension {"L" 1}]}}
                            [:arg-list [:arg [:name "x"] [:name "L"]]])))

    (is (= (infer-args-list {:types {"type" :type}}
                            [:arg-list
                             [:arg [:name "x"] [:type-name "type"]]])
           [{"x" :type :types {"type" :type}} [:product :type]]))))

(deftest core-infer-funcs
  (testing "infer-function"
    (let [[_ functype] (infer-function
                        {}
                        [:function [:arg-list [:arg [:name "x"]]] [:name "x"]])
          x-type (last functype)]
      (is (= functype [:function [:product x-type] x-type])))

    (is (infer-function {"y" :type}
                        [:function [:arg-list [:arg [:name "x"]]] [:name "y"]])))

  (testing "infer-def"
    (is (= (infer-def {}
                      [:def [:name "x"] [:number 1]])
           [{} [:dimension {}]]))

    (is (infer-def {"*" [:forall #{'d__1 'd__2}
                         [:function [:product
                                     [:dimension {'d__1 1}]
                                     [:dimension {'d__2 1}]]
                          [:dimension {'d__1 1 'd__2 1}]]]}
                   [:func-def [:name "f"] [:arg-list [:arg [:name "x"]]]
                    [:apply [:name "*"] [:name "x"] [:name "x"]]])))

  (testing "infer-let"
    (is (= (infer-let {}
                      [:let [:def [:name "x"] [:number 1]] [:name "x"]])
           [{} [:dimension {}]])))
  (testing "infer-if"
    (is (= (infer-if {"x" :type "y" :type}
                     [:if [:boolean "True"] [:name "x"] [:name "y"]])
           [{} :type])))

  (testing "infer-apply"
    (is (= (second (infer-apply {"*" [:forall #{'d__1 'd__2}
                                      [:function [:product
                                                  [:dimension {'d__1 1}]
                                                  [:dimension {'d__2 1}]]
                                       [:dimension {'d__1 1 'd__2 1}]]]}
                                [:apply [:name "*"] [:number 1] [:number 1]]))
           [:dimension {}]))))

(deftest expressions

  (testing "strange multiplication behaviour"
    (is (= (second (infer {"m" [:dimension {"m" 1}]
                           "*" [:forall #{'d__1 'd__2}
                                [:function [:product
                                            [:dimension {'d__1 1}]
                                            [:dimension {'d__2 1}]]
                                 [:dimension {'d__1 1 'd__2 1}]]]
                           "sqrt" [:forall #{'d__1}
                                   [:function [:product [:dimension {'d__1 2}]]
                                    [:dimension {'d__1 1}]]]}
                          [:apply [:name "*"]
                           [:apply [:name "*"]
                            [:apply [:name "*"] [:name "m"] [:name "m"]]
                            [:name "m"]]
                           [:name "m"]]))
           [:dimension {"m" 4}])))

  (testing "sqrt function"
    (is (= (second (infer {"m" [:dimension {"m" 1}]
                           "*" [:forall #{'d__1 'd__2}
                                [:function [:product
                                            [:dimension {'d__1 1}]
                                            [:dimension {'d__2 1}]]
                                 [:dimension {'d__1 1 'd__2 1}]]]
                           "sqrt" [:forall #{'d__1}
                                   [:function [:product [:dimension {'d__1 2}]]
                                    [:dimension {'d__1 1}]]]}
                          [:apply [:name "sqrt"]
                           [:apply [:name "*"] [:name "m"] [:name "m"]]]))
           [:dimension {"m" 1}]))

    (is (= (second (infer {"m" [:dimension {"m" 1}]
                           "*" [:forall #{'d__1 'd__2}
                                [:function [:product
                                            [:dimension {'d__1 1}]
                                            [:dimension {'d__2 1}]]
                                 [:dimension {'d__1 1 'd__2 1}]]]
                           "sqrt" [:forall #{'d__1}
                                   [:function [:product [:dimension {'d__1 2}]]
                                    [:dimension {'d__1 1}]]]}
                          [:apply [:name "sqrt"]
                           [:apply [:name "*"]
                            [:apply [:name "*"] [:name "m"] [:name "m"]]
                            [:apply [:name "*"] [:name "m"] [:name "m"]]]]))
           [:dimension {"m" 2}]))

    (is (= (second (infer {"m" [:dimension {"m" 1}]
                           "*" [:forall #{'d__1 'd__2}
                                [:function [:product
                                            [:dimension {'d__1 1}]
                                            [:dimension {'d__2 1}]]
                                 [:dimension {'d__1 1 'd__2 1}]]]
                           "sqrt" [:forall #{'d__1}
                                   [:function [:product [:dimension {'d__1 2}]]
                                    [:dimension {'d__1 1}]]]}
                          [:apply [:name "sqrt"]
                           [:apply [:name "*"]
                            [:apply [:name "*"]
                             [:apply [:name "*"] [:name "m"] [:name "m"]]
                             [:name "m"]]
                            [:name "m"]]]))
           [:dimension {"m" 2}]))))

#_(deftest basic-exprs
    (testing "Expressions"
      (is (= (p/infer-expr "m")
             [:dimension {"L" 1}]))
      (is (= (p/infer-expr "m / s")
             [:dimension {"L" 1 "T" -1}]))
      (is (= (p/infer-expr "20 kg")
             [:dimension {"M" 1}]))

      (is (= (p/infer-expr "20 m / (2 s) == 10 m/s")
             [:bool]))
      (is (= (p/infer-expr "fn [x] x x")
             [:function [:product [:dimension {[:dim-variable 2] 1}]]
              [:dimension {[:dim-variable 2] 2}]]))
      (is (= (p/infer-expr "let x = 1m in x")
             [:dimension {"L" 1}]))))
