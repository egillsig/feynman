(ns feynman.dimensions-test
  (:require [clojure.test :refer :all]
            [feynman.dimensions :refer :all]
            [feynman.state :as state]))

(deftest dim-seq-ops
  (testing "create-dimvec"
    (is (= (create-dimvec {"L" 0 "M" 1})
           {"M" 1})))

  (testing "map-vals"
    (is (= (map-vals inc {"L" 1 "M" 2 [:dim-variable 1] 4})
           {"L" 2 "M" 3 [:dim-variable 1] 5})))

  (testing "filter-keys"
    (is (= (filter-keys #(= % "M") {"L" 1 "M" 2 [:dim-variable 1] 4})
           {"M" 2})))

  (testing "remove-keys"
    (is (= (remove-keys #(= % "M") {"L" 1 "M" 2 [:dim-variable 1] 4})
           {"L" 1 [:dim-variable 1] 4})))
  (testing "split-by"
    (is (= (split-by #(= % "M") {"L" 1 "M" 2 [:dim-variable 1] 4})
           [{"M" 2} {"L" 1 [:dim-variable 1] 4}])))

  (testing "Get dimension variables"
    (is  (= (dim-variables {"L" 1 "M" 2 [:dim-variable 1] 4})
            {[:dim-variable 1] 4})))

  (testing "Minimum exponent variable"
    (is (= (min-exponent-var {"L" 1 [:dim-variable 1] 3 [:dim-variable 2] 2 "M" 4})
           [[:dim-variable 2] 2]))
    (is (= (min-exponent-var {"L" -1 [:dim-variable 1] -3 [:dim-variable 2] -2 "M" 4})
           [[:dim-variable 2] -2]))))

(deftest dim-arithmetic-ops

  (testing "Add dimension vectors"
    (is (= (add {"L" 1 "M" 2} {"M" 2 [:dim-variable 1] 2})
           {"L" 1 "M" 4 [:dim-variable 1] 2})))

  (testing "Multiply dimension vectors"
    (is (= (mul {"L" 1 "M" 2} 2)
           {"L" 2 "M" 4}))
    (is (= (mul {"L" 1 "M" 2} (/ 1 2))
           {"M" 1})))

  (testing "Invert dimension vectors"
    (is (= (inv {"L" 1 "M" 2 [:dim-variable 1] 4})
           {"L" -1 "M" -2 [:dim-variable 1] -4})))

  (testing "Dimension vector divides?"
    (is (divides? {"L" 2 "M" 4 [:dim-variable 1] 6}
                  2))
    (is (not (divides? {"L" 2 "M" 4 [:dim-variable 1] 6}
                       3)))))

(deftest substitutions

  (testing "apply-subs"

    (let [s {[:dim-variable 1] {}
             [:dim-variable 2] {"L" 1 "T" -2}
             [:dim-variable 3] {"M" 1}
             [:dim-variable 4] {[:dim-variable 3] 2}}]
      (is (= (apply-subs s {[:dim-variable 1] 4})
             {}))
      (is (= (apply-subs s {[:dim-variable 2] 1})
             {"L" 1 "T" -2}))
      (is (= (apply-subs s {[:dim-variable 2] 2})
             {"L" 2 "T" -4}))
      (is (= (apply-subs s {[:dim-variable 2] 1 [:dim-variable 3] -1})
             {"L" 1 "T" -2 "M" -1}))
      (is (= (apply-subs s {[:dim-variable 4] 2})
             {"M" 4}))
      (is (= (apply-subs s {[:dim-variable 5] 1})
             {[:dim-variable 5] 1})))))

(deftest unification
  (testing "dim-unify-one"

    (let [state (state/create-state :dims 3)]
      (is (= (unify-with-one {} state)
             {}))

      (is (= (unify-with-one {[:dim-variable 1] 1} state)
             {[:dim-variable 1] {}}))

      (is (= (unify-with-one {[:dim-variable 1] 1 "L" 1} state)
             {[:dim-variable 1] {"L" -1}}))

      ;; If one variable can dominate it should:
      (is (= (unify-with-one {[:dim-variable 1] 1 [:dim-variable 2] 2 "L" 2}
                             state)
             {[:dim-variable 1] {[:dim-variable 2] -2 "L" -2}}))
      (is (= (unify-with-one {[:dim-variable 1] 2 [:dim-variable 2] 4 "L" 2 "M" 4}
                             state)
             {[:dim-variable 1] {[:dim-variable 2] -2 "L" -1 "M" -2}}))

      ;; Introduces new dim-variable:
      ;; d_1^2 * d_2^3 * L * M^2
      ;;  d_1 -> d_2^-1 * d_4 * M^-1
      ;;  d_2 -> d_4^-2 * L^-1
      (is (= (unify-with-one {[:dim-variable 1] 2 [:dim-variable 2] 3 "L" 1 "M" 2}
                             state)
             {[:dim-variable 1] {[:dim-variable 2] -1 [:dim-variable 4] 1 "M" -1}
              [:dim-variable 2] {[:dim-variable 4] -2 "L" -1}}))

      (let [d {[:dim-variable 1] 2
               [:dim-variable 2] 3
               [:dim-variable 3] 4
               "B_1" 1 "B_2" 2 "B_3" 5}]
        ;; This property should always hold
        (is (= {} (apply-subs (unify-with-one d state) d))))

      ;; Some impossible examples:
      (is (nil? (unify-with-one {"L" 2} state)))
      (is (nil? (unify-with-one {[:dim-variable 1] 2 "L" 1} state))))))
