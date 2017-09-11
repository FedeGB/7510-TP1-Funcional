(ns utilities-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))


(deftest various-utilities-test
  (testing "Put together utility function test"
    (is (= (putTogether "padre(juan, pepe)")
           "padrejuanpepe"))
  )
  (testing "Obtain values from within brackets test"
    (def vars (obtainValuesFromBrackets "padre(juan,pepe)"))
    (is (= (get vars 0)
           "juan"))
    (is (= (get vars 1)
           "pepe"))
  )
  (testing "Generate rule list test"
    (def list (generateRuleList "rule(X,Y)" "fact1(X),fact2(Y,X)"))
    (is (= (get list 0)
           2))
    (is (= (get list 1)
           "X"))
    (is (= (get list 2)
           "Y"))
    (is (= (get list 3)
           "fact1X"))
    (is (= (get list 4)
           "fact2YX"))
  )
)
