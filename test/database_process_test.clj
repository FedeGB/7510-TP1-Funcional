(ns database-process-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(deftest database-fact-process-test
  (testing "Process a valid fact with single value"
    (processValidFact "varon(juan)" factMap)
    (is (= (contains? factMap "varonjuan")
           true))
    )
  (testing "Process a valid fact with two values"
    (processValidFact "padre(juan, pepe)" factMap)
    (is (= (contains? factMap "padrejuanpepe")
           true))
  )
)

(deftest database-rule-process-test
  (testing "Process a valid rule, rule is contained in map"
    (processValidRule "hijo(X, Y) :- varon(X), padre(Y, X)" ruleMap)
    (is (= (contains? ruleMap "hijo")
           true))
  )
  (testing "Process a valid rule, correct rule structure"
    (processValidRule "hijo(X, Y) :- varon(X), padre(Y, X)" ruleMap)
    (def ruleData (get ruleMap "hijo"))
    (is (= (get ruleData 0)
           2))
    (is (= (get ruleData 1)
           "X"))
    (is (= (get ruleData 2)
           "Y"))
    (is (= (get ruleData 3)
           "varonX"))
    (is (= (get ruleData 4)
           "padreYX"))
  )
)

(deftest database-element-process-test
  (testing "Process a valid rule as element"
    (operateInputElement "hija(X, Y, Z) :- mujer(X, Z), padre(Y, X)" factMap ruleMap)
    (is (= (contains? ruleMap "hija")
           true))
    (def ruleData (get ruleMap "hija"))
    (is (= (get ruleData 0)
           3))
    (is (= (get ruleData 1)
           "X"))
    (is (= (get ruleData 2)
           "Y"))
    (is (= (get ruleData 3)
           "Z"))
    (is (= (get ruleData 4)
           "mujerXZ"))
    (is (= (get ruleData 5)
           "padreYX"))
  )
  (testing "Process a valid fact"
    (operateInputElement "padre(juan, papo)" factMap ruleMap)
    (is (= (contains? factMap "padrejuanpapo")
           true))
  )
)