(ns query-process-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(deftest query-process-entry-test
    (testing "Case of rule replace and matching"
      (def replacingValue {"X" "test1", "Y" "test2"})
      (def factMapTest {})
      (def factMapTest (assoc factMapTest "facttest1test2" 1))
      (is (= (replaceAndMatch factMapTest replacingValue "factXY")
             true))
    )
    (testing "Case of rule replace and not matching"
      (def replacingValue {"X" "test1", "Y" "test3"})
      (def factMapTest {})
      (def factMapTest (assoc factMapTest "facttest1test2" 1))
      (is (= (replaceAndMatch factMapTest replacingValue "factXY")
             false))
    )
    (testing "ProcessQuery test with fact. No rule on DB, will not check"
      (def query "fact(test1,test2)")
      (operateInputElement "fact(test1,test2)" factMap ruleMap)
      (is (= (processQuery query factMap ruleMap)
             true))
    )
    (testing "ProcessQuery test with rule."
      (def query "rule(test1,test2)")
      (operateInputElement "fact(test1,test2)" factMap ruleMap)
      (operateInputElement "rule(X,Y) :- fact(X,Y)" factMap ruleMap)
      (is (= (processQuery query factMap ruleMap)
             true))
    )
    (testing "ProcessQuery test with fact. No rule on DB, will not check. False case."
      (def query "fact(test1)")
      (operateInputElement "fact(test1,test2)" factMap ruleMap)
      (is (= (processQuery query factMap ruleMap)
             false))
    )
    (testing "ProcessQuery test with rule. False case, extra variable."
      (def query "rule(test1,test2,test3)")
      (operateInputElement "fact(test1,test2)" factMap ruleMap)
      (operateInputElement "rule(X,Y) :- fact(X,Y)" factMap ruleMap)
      (is (= (processQuery query factMap ruleMap)
             false))
    )
)