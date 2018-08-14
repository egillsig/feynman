(ns feynman.dimensions-test
  (:require [clojure.test :refer :all]
            [feynman.dimensions :refer :all]))

(deftest dim-seq-ops
  (testing "create-vec"
    (is (= (create-vec {"L" 0 "M" 1})
           {"M" 1})))

  (testing "map-vals"
    (is (= (vmap inc {"L" 1 "M" 2 'd 4})
           {"L" 2 "M" 3 'd 5})))

  (testing "filter-keys"
    (is (= (filter-keys #(= % "M") {"L" 1 "M" 2 'd 4})
           {"M" 2})))

  (testing "remove-keys"
    (is (= (remove-keys #(= % "M") {"L" 1 "M" 2 'd 4})
           {"L" 1 'd 4})))
  (testing "split-by"
    (is (= (split-by #(= % "M") {"L" 1 "M" 2 'd 4})
           [{"M" 2} {"L" 1 'd 4}])))

  (testing "Get dimension variables"
    (is  (= (variables {"L" 1 "M" 2 'd 4})
            {'d 4})))

  (testing "Minimum exponent variable"
    (is (= (min-exponent-var {"L" 1 'd__1 3 'd__2 2 "M" 4})
           ['d__2 2]))
    (is (= (min-exponent-var {"L" -1 'd__1 -3 'd__2 -2 "M" 4})
           ['d__2 -2]))))

(deftest dim-arithmetic-ops

  (testing "Add dimension vectors"
    (is (= (add {"L" 1 "M" 2} {"M" 2 'd 2})
           {"L" 1 "M" 4 'd 2})))

  (testing "Multiply dimension vectors"
    (is (= (mul {"L" 1 "M" 2} 2)
           {"L" 2 "M" 4}))
    (is (= (mul {"L" 1 "M" 2} (/ 1 2))
           {"M" 1})))

  (testing "Invert dimension vectors"
    (is (= (inv {"L" 1 "M" 2 'd 4})
           {"L" -1 "M" -2 'd -4})))

  (testing "Dimension vector divides?"
    (is (divides? {"L" 2 "M" 4 'd 6}
                  2))
    (is (not (divides? {"L" 2 "M" 4 'd 6}
                       3)))))

(deftest substitutions

  (testing "apply-subst"

    (let [s {'d1 {}
             'd2 {"L" 1 "T" -2}
             'd3 {"M" 1}
             'd4 {'d3 2}}]
      (is (= (apply-subst s {'d1 4})
             {}))
      (is (= (apply-subst s {'d2 1})
             {"L" 1 "T" -2}))
      (is (= (apply-subst s {'d2 2})
             {"L" 2 "T" -4}))
      (is (= (apply-subst s {'d2 1 'd3 -1})
             {"L" 1 "T" -2 "M" -1}))
      (is (= (apply-subst s {'d4 2})
             {"M" 4}))
      (is (= (apply-subst s {'d5 1})
             {'d5 1})))))

(deftest unification
  (testing "dim-unify-one"
    (is (= (unify-with-one {}) {}))

    (is (= (unify-with-one {'d 1}) {'d {}}))

    (is (= (unify-with-one {'d__1 1 "L" 1})
           {'d__1 {"L" -1}}))

    ;; If one variable can dominate it should:
    (is (= (unify-with-one {'d__1 1 'd__2 2 "L" 2})
           {'d__1 {'d__2 -2 "L" -2}}))
    (is (= (unify-with-one {'d__1 2 'd__2 4 "L" 2 "M" 4})
           {'d__1 {'d__2 -2 "L" -1 "M" -2}}))

    ;; Introduces new dim-variable:
    ;; d_1^2 * d_2^3 * L * M^2
    ;;  d_1 -> d_2^-1 * d_4 * M^-1
    ;;  d_2 -> d_4^-2 * L^-1
    (is (unify-with-one {'d__1 2 'd__2 3 "L" 1 "M" 2}))

    (let [d {'d__1 2
             'd__2 3
             'd__3 4
             "B_1" 1 "B_2" 2 "B_3" 5}]
      ;; This property should always hold
      (is (= {} (apply-subst (unify-with-one d) d))))

    ;; Some impossible examples:
    (is (nil? (unify-with-one {"L" 2})))
    (is (nil? (unify-with-one {'d 2 "L" 1})))))
