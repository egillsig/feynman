(ns feynman.parser-test
  (:require [feynman.parser :refer :all]
            [clojure.test :refer :all]
            [instaparse.core :as insta]))

(defn strip-enlive
  [n]
  (if (or (keyword? n) (string? n)) n
      (condp = (count n)
        1 n
        2 (strip-enlive (second n))
        (map strip-enlive n))))

(defn parse-and-strip
  [e]
  (-> e parse))

(deftest exprs
  (testing "Unary +/-"
    (is (= (parse-and-strip "1+1") (parse-and-strip "1+(1)")))
    (is (= (parse-and-strip "1-1") (parse-and-strip "1-(1)")))
    (is (= (parse-and-strip "-1-1") (parse-and-strip "(-1)-1")))
    (is (= (parse-and-strip "-1-1") (parse-and-strip "-1-(1)"))))

  (testing "Parenthesis imply multiplication"
    (is (not (= (parse-and-strip "1-1") (parse-and-strip "1(-1)")))))

  (testing "Precedence"
    (is (= (parse-and-strip "1*2+3") (parse-and-strip "(1*2)+3")))
    (is (not (= (parse-and-strip "1*2+3") (parse-and-strip "1*(2+3)"))))
    (is (= (parse-and-strip  "1 2+3") (parse-and-strip "(1 2)+3")))
    (is (not (= (parse-and-strip "1 2+3") (parse-and-strip "1 (2+3)")))))

  (testing "Left-associative binary operators"
    (is (= (parse-and-strip "1 + 2 + 3") (parse-and-strip "(1 + 2) + 3")))
    (is (= (parse-and-strip "1 - 2 - 3") (parse-and-strip "(1 - 2) - 3")))
    (is (= (parse-and-strip "1 2 3") (parse-and-strip "(1 2) 3")))
    (is (= (parse-and-strip "1 * 2 * 3") (parse-and-strip "(1 * 2) * 3")))
    (is (= (parse-and-strip "1 / 2 / 3") (parse-and-strip "(1 / 2) / 3"))))

  ;; Exponentiation is right-associative
  (testing "Exponentiation"
    (is (= (parse-and-strip "1^2^3") (parse-and-strip "1^(2^3)")))
    (is (= (parse-and-strip "1^-2") (parse-and-strip "1^(-2)")))
    (is (= (parse-and-strip "1^-2^3") (parse-and-strip "1^-(2^3)")))
    (is (= (parse-and-strip "1^2 3") (parse-and-strip "(1^2) 3"))))

  (testing "Function application"
    (is (= (parse-and-strip "f[x] + 1") (parse-and-strip "(f[x]) + 1")))))

(deftest compound
  (testing "Compound expressions"
    (is (= (parse-and-strip "fn [x] x + 1")
           (parse-and-strip "fn [x] (x + 1)")))
    (is (not= (parse-and-strip "(fn [x] x) + 1")
              (parse-and-strip  "fn [x] (x + 1)")))

    (is (= (parse-and-strip "if cond then x else x + 1")
           (parse-and-strip "if cond then x else (x + 1)")))
    (is (not= (parse-and-strip "(if cond then x else x) + 1")
              (parse-and-strip "if cond then x else (x + 1)")))))

(deftest type-exprs
  (testing "Type expressions"
    ;; Verify that it doesn't throw error
    (is (parse-and-strip "def x : M = 1"))
    (is (parse-and-strip "def x : L / T = 1"))
    (is (parse-and-strip "def x : M * Velocity = 1"))
    (is (parse-and-strip "def x : M Vel = 1"))
    (is (parse-and-strip "def x : M ^ 2 = 1"))
    (is (parse-and-strip "def f[x: M, y: L^2, z]: T = 1"))
    (is (parse-and-strip "let x: M = 1 in x x"))
    (is (parse-and-strip "let f[x,y]: Dim = x y in f[1,1]"))
    (is (parse-and-strip "def x : A^2 = 1"))
    (is (parse-and-strip "def x : A -> B = 1"))
    (is (parse-and-strip "def integrate[f: A -> B, a: A, b: B, n: 1]: A * B = 1"))))
