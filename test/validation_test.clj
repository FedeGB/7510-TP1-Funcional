(ns validation-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))


(deftest validation-rule-structure-test
  (testing "Rule structure validation with two facts to evaluate"
    (is (= (isValidRule "hijo(X, Y) :- varon(X), padre(Y, X)")
           true))
  )
  (testing "Rule structure validation with single fact"
    (is (= (isValidRule "hijx(X, Y) :- padre(Y, X)")
           true))
  )
  (testing "Rule structure validation with three facts"
    (is (= (isValidRule "hijo(X, Y) :- padre(Y, X), varon(X), relacionado(X,Y)")
           true))
  )
  (testing "Rule structure validation with single variable"
    (is (= (isValidRule "hijo(X) :- varon(X)")
           true))
  )
  (testing "Rule structure validation, invalid 1"
    (is (= (isValidRule "hijo(X) - varon(X)")
           false))
  )
  (testing "Rule structure validation, invalid 2"
    (is (= (isValidRule "hijo(X :- varon(X)")
           false))
  )
  (testing "Rule structure validation, invalid 3"
    (is (= (isValidRule "hijo(X) :- varon(X),")
           false))
  )
)

(deftest validation-fact-structure-test
  (testing "Fact structure validation with single value"
    (is (= (isValidFact "varon(juan)")
           true))
  )
  (testing "Fact structure validation with various values"
    (is (= (isValidFact "padre(pepe, juan)")
           true))
  )
  (testing "Fact structure validation invalid 1"
    (is (= (isValidFact "padre(pepe, juan")
           false))
  )
  (testing "Fact structure validation invalid 2"
    (is (= (isValidFact "padre pepe, juan)")
           false))
  )
)

(deftest validation-query-structure-test
  (testing "Query structure validation with single value"
    (is (= (isValidQuery "varon(juan)")
           true))
  )
  (testing "Query structure validation with various values"
    (is (= (isValidQuery "padre(pepe, juan)")
           true))
  )
  (testing "Query structure validation invalid 1"
    (is (= (isValidQuery "padrepepe, juan)")
           false))
  )
  (testing "Query structure validation invalid 2"
    (is (= (isValidQuery "padre(pepe, juan")
           false))
  )
)