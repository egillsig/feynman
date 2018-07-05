(ns feynman.parser-test
  (:require [feynman.parser :refer :all]
            [clojure.test :refer :all]
            [instaparse.core :as insta]))

(def parse-and-strip (comp strip-enlive parser))

(defn equiv?
  "Are the two expressions equivalent up to useless nesting?"
  [e1 e2] (= (parse-and-strip e1) (parse-and-strip e2)))

(def not-equiv? (complement equiv?))

(deftest exprs
  (testing "Arithmetic expressions"
    (is (equiv? "1+1" "1+(1)"))
    (is (equiv? "1-1" "1-(1)"))
    (is (equiv? "-1-1" "(-1)-1"))
    (is (equiv? "-1-1" "-1-(1)"))

    ;; Parens imply multiplication
    (is (not-equiv? "1-1" "1(-1)"))

    ;; Precedence
    (is (equiv? "1*2+3" "(1*2)+3"))
    (is (not-equiv? "1*2+3" "1*(2+3)"))
    (is (equiv? "1 2+3" "(1 2)+3"))
    (is (not-equiv? "1 2+3" "1 (2+3)"))

    ;; Most binary operators are left-associative
    (is (equiv? "1 + 2 + 3" "(1 + 2) + 3"))
    (is (equiv? "1 - 2 - 3" "(1 - 2) - 3"))
    (is (equiv? "1 2 3" "(1 2) 3"))
    (is (equiv? "1 * 2 * 3" "(1 * 2) * 3"))
    (is (equiv? "1 / 2 / 3" "(1 / 2) / 3"))

    ;; Exponentiation is right-associative
    (is (equiv? "1^2^3" "1^(2^3)"))
    (is (equiv? "1^-2" "1^(-2)"))
    (is (equiv? "1^-2^3" "1^-(2^3)"))
    (is (equiv? "1^2 3" "(1^2) 3"))

    ;; Function application binds tightly
    (is (equiv? "f[x] + 1" "(f[x]) + 1"))))

(deftest compound
  (testing "Compound expressions"
    (is (equiv? "fn [x] x + 1" "fn [x] (x + 1)"))
    (is (not-equiv? "(fn [x] x) + 1"  "fn [x] (x + 1)"))

    (is (equiv? "if cond then x else x + 1" "if cond then x else (x + 1)"))
    (is (not-equiv? "(if cond then x else x) + 1" "if cond then x else (x + 1)"))))
