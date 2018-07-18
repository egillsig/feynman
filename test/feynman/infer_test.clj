(ns feynman.infer-test
  (:require [clojure.test :refer :all]
            [feynman.env :as env]
            [feynman.infer :refer :all]))

(deftest basic-infer-funcs
  (testing "infer-name"
    (is (= (infer-name {"m" :type} [:name "m"]) [{} :type]))

    (is (infer-name {"square" [:forall #{'d__1}
                               [:function [:dimension {'d__1 1}]
                                [:dimension {'d__1 2}]]]}
                    [:name "square"])))

  (testing "infer-args-list"
    (is (= (infer-args-list {"L" :type}
                            [:arg-list
                             [:arg [:name "x"] [:name "L"]]])
           [{"x" :type} [:product :type]]))))

(deftest core-infer-funcs
  (testing "infer-function"
    (is (infer-function
         {}
         [:function [:arg-list [:arg [:name "x"]]] [:name "x"]]))

    (is (infer-function {"y" :type}
                        [:function [:arg-list [:arg [:name "x"]]] [:name "y"]])))

  (testing "infer-def"
    (is (= (infer-def {}
                      [:def [:name "x"] [:number "1"]])
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
                      [:let [:def [:name "x"] [:number "1"]] [:name "x"]])
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
                                [:apply [:name "*"] [:number "1"] [:number "1"]]))
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
