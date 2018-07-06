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
  (-> e parse strip-enlive))

(deftest exprs
  (testing "Arithmetic expressions"
    (is (= (parse-and-strip "1+1") (parse-and-strip "1+(1)")))
    (is (= (parse-and-strip "1-1") (parse-and-strip "1-(1)")))
    (is (= (parse-and-strip "-1-1") (parse-and-strip "(-1)-1")))
    (is (= (parse-and-strip "-1-1") (parse-and-strip "-1-(1)")))

    ;; Parens imply multiplication
    (is (not (= (parse-and-strip "1-1") (parse-and-strip "1(-1)"))))

    ;; Precedence
    (is (= (parse-and-strip "1*2+3") (parse-and-strip "(1*2)+3")))
    (is (not (= (parse-and-strip "1*2+3") (parse-and-strip "1*(2+3)"))))
    (is (= (parse-and-strip  "1 2+3") (parse-and-strip "(1 2)+3")))
    (is (not (= (parse-and-strip "1 2+3") (parse-and-strip "1 (2+3)"))))

    ;; Most binary operators are left-associative
    (is (= (parse-and-strip "1 + 2 + 3") (parse-and-strip "(1 + 2) + 3")))
    (is (= (parse-and-strip "1 - 2 - 3") (parse-and-strip "(1 - 2) - 3")))
    (is (= (parse-and-strip "1 2 3") (parse-and-strip "(1 2) 3")))
    (is (= (parse-and-strip "1 * 2 * 3") (parse-and-strip "(1 * 2) * 3")))
    (is (= (parse-and-strip "1 / 2 / 3") (parse-and-strip "(1 / 2) / 3")))

    ;; Exponentiation is right-associative
    (is (= (parse-and-strip "1^2^3") (parse-and-strip "1^(2^3)")))
    (is (= (parse-and-strip "1^-2") (parse-and-strip "1^(-2)")))
    (is (= (parse-and-strip "1^-2^3") (parse-and-strip "1^-(2^3)")))
    (is (= (parse-and-strip "1^2 3") (parse-and-strip "(1^2) 3")))

    ;; Function application binds tightly
    (is (= (parse-and-strip "f[x] + 1") (parse-and-strip "(f[x]) + 1")))))

(deftest compound
  (testing "Compound expressions"
    (is (= (parse-and-strip "fn [x] x + 1")
           (parse-and-strip "fn [x] (x + 1)")))
    (is (not (= (parse-and-strip "(fn [x] x) + 1")
                (parse-and-strip  "fn [x] (x + 1)"))))

    (is (= (parse-and-strip "if cond then x else x + 1")
           (parse-and-strip "if cond then x else (x + 1)")))
    (is (not (= (parse-and-strip "(if cond then x else x) + 1")
                (parse-and-strip "if cond then x else (x + 1)"))))))
