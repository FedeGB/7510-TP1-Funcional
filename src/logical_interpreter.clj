(ns logical-interpreter)
(require '[clojure.string :as str])

(def factMap {})
(def ruleMap {})

(defn isValidRule
  [entry]
  (not (nil? (re-find #"^[^\(]*\([^)]*\) :- ([^\(]*\([^)]*\), *)*([^\(]*\([^)]*\))$" entry)))
  )

(defn isValidFact
  [entry]
  (not (nil? (re-find #"^[^\(]*\([^)]*\)$" entry)))
  )

(defn isValidQuery
  [entry]
  (not (nil? (re-find #"^[^\(]*\([^)]*\)$" entry)))
  )

(defn putTogether
  [input]
  (str/replace input #"( *\( *| *\) *| *, *)" "")
  )

(defn processValidFact
  [fact factMap]
  (def allTogether (putTogether fact))
  (def factMap (assoc factMap  allTogether 1))
  true
  )

(defn obtainValuesFromBrackets
  [input]
  (str/split (str/replace (get (str/split input #"\(") 1) #"\)" "") #",")
)

(defn generateRuleList
  [ruleSide factSide]
  (def variables (obtainValuesFromBrackets ruleSide))
  (def processedFacts (map putTogether (str/split factSide #"\) *,")))
  (def ruleList [(count variables)])
  (def ruleList (into [] (concat ruleList variables)))
  (def ruleList (into [] (concat ruleList processedFacts)))
  ruleList
)

(defn processValidRule
  [rule ruleMap]
  (def separated (str/split (str/replace rule #" " "") #":-"))
  (def ruleSide (get separated 0))
  (def factSide (get separated 1))
  (def ruleName (get (str/split ruleSide #"\(") 0))
  (def ruleList (generateRuleList ruleSide factSide))
  (def ruleMap (assoc ruleMap ruleName ruleList))
  true
  )

(defn operateInputElement
  [element factMap ruleMap]
  (if (isValidRule element)
    (def validRule (processValidRule element ruleMap))
    (def validRule false)
    )
  (if (isValidFact element)
    (def validFact (processValidFact element factMap))
    (def validFact false)
    )
  (or validFact validRule)
  )


(defn replaceAndMatch
  [factMap replacingValues ruleFact]
  (def ruleFactNew ruleFact)
  (doseq [value (keys replacingValues)]
    (def ruleFactNew (str/replace ruleFactNew (re-pattern value) (get replacingValues value)))
    )
  (contains? factMap ruleFactNew)
  )

(defn evaluateFact
  [factMap ruleConditions ruleValues]
  (def replacingValues {})
  (def iterateRangeValues (into [] (take (count ruleValues) (iterate inc 1))))
  (doseq [it iterateRangeValues]
    (def replacingValues (assoc replacingValues (get ruleConditions it) (get ruleValues (- it 1))))
    )
  (def ruleEvaluation true)
  (def inValues (get ruleConditions 0))
  (def iterateRangeFacts (into [] (take (- (count ruleConditions) (+ inValues 1)) (iterate inc (+ inValues 1)))))
  (doseq [it2 iterateRangeFacts]
    (def ruleEvaluation (and ruleEvaluation (replaceAndMatch factMap replacingValues (get ruleConditions it2))))
    )
  ruleEvaluation
  )

(defn evaluateFactsOnRule
  [factMap ruleConditions ruleValues]
  (def inValues (get ruleConditions 0))
  (if (not= (count ruleValues) inValues)
    false
    (evaluateFact factMap ruleConditions ruleValues)
    )
  )

(defn evaluateQueryRule
  [query factMap ruleMap]
  (def ruleName (get (str/split query #"\(") 0))
  (def ruleValues (obtainValuesFromBrackets query))
  (if (contains? ruleMap ruleName)
    (evaluateFactsOnRule factMap (get ruleMap ruleName) ruleValues)
    false
    )
  )

(defn processQuery
  [query factMap ruleMap]
  (def queryAsFact (putTogether query))
  (if (contains? factMap queryAsFact)
    true
    (evaluateQueryRule query factMap ruleMap)
    )
  )

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (def inputParsed (str/split (str/replace database #"(\t|\n)" ".") #"\.+"))
  (def queryClean (str/replace query #" *" ""))
  (def validInput true)
  (doseq [element inputParsed]
    (def validInput (and (operateInputElement element factMap ruleMap) validInput))
  )
  (if validInput
    (if (isValidQuery queryClean)
      (def result (processQuery queryClean factMap ruleMap))
      (def result nil)
    )
    (def result nil)
  )
  result
)