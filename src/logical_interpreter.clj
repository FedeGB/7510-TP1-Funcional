(ns logical-interpreter)
(require '[clojure.string :as str])

(def factMap {})
(def ruleMap {})

(defn isValidRule
  "Returns true if the format of the fact is valid, false otherwise.
  Valid example: 'rule(X,Y) :- fact1(X), fact2(Y,X)'"
  [entry]
  (not (nil? (re-find #"^[^\(]*\([^)]*\) :- ([^\(]*\([^)]*\), *)*([^\(]*\([^)]*\))$" entry)))
  )

(defn isValidFact
  "Returns true if the format of the fact is valid, false otherwise.
  Valid example: 'fact(var1,var2)'"
  [entry]
  (not (nil? (re-find #"^[^\(]*\([^)]*\)$" entry)))
  )

(defn isValidQuery
  "Returns true if the format of the query is valid, false otherwise.
  Valid example: 'query(var1,var2)'"
  [entry]
  (not (nil? (re-find #"^[^\(]*\([^)]*\)$" entry)))
  )

(defn putTogether
  "Removes '(' ')' and ',' from the input string. For example:
  fact(x,y) is processed as 'factxy'"
  [input]
  (str/replace input #"( *\( *| *\) *| *, *)" "")
  )

(defn processValidFact
  "Process a valid fact and stores it in a set. The fact gets processed as following:
  fact(x,y) is processed as 'factxy'"
  [fact factMap]
  (def allTogether (putTogether fact))
  (def factMap (assoc factMap  allTogether 1))
  true
  )

(defn obtainValuesFromBrackets
  "Returns in a list the variables that are contained between brackets,
  separated by commas. For example:
  fact(x,y) returns [\"x\" \"y\"]"
  [input]
  (str/split (str/replace (get (str/split input #"\(") 1) #"\)" "") #",")
)

(defn generateRuleList
  "Generate the rule list with facts and variables data. The final list contains for example:
  For a given rule input \"rule(X,Y) :- fact(X,Y)\"
  [2 \"X\" \"Y\" \"factXY\"] where
  - 2 is the number of variables that the rule contains
  - Followed by the variables
  - And the last part with the facts, without '(' ')' and ','"
  [ruleSide factSide]
  (def variables (obtainValuesFromBrackets ruleSide))
  (def processedFacts (map putTogether (str/split factSide #"\) *,")))
  (def ruleList [(count variables)])
  (def ruleList (into [] (concat ruleList variables)))
  (def ruleList (into [] (concat ruleList processedFacts)))
  ruleList
)

(defn processValidRule
  "Rule processing part of database input. This will store in a set
  the rule by name with a list of information that the rule gives
  about it's facts and variables (see more on 'generateRuleList'."
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
  "First level of operation of the database input. It also validates if the
  element is a valid Fact or valid Rule, if not this will return false, if it is
  this return true."
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
  "Fifth and final level of query processing. Try to replace The mapped variables with query values
  and match with facts that we have. If th fact matches this returns true, else it returns false"
  [factMap replacingValues ruleFact]
  (def ruleFactNew ruleFact)
  (doseq [value (keys replacingValues)]
    (def ruleFactNew (str/replace ruleFactNew (re-pattern value) (get replacingValues value)))
    )
  (contains? factMap ruleFactNew)
  )

(defn evaluateFact
  "Fourth level of query processing. At this level the value on the query and the variable on the
  stored rule get mapped. This just prepares for the evaluation of the facts on the rules."
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
  "Third level of query processing. The second part of the rule evaluation,
  if the quantity of variables on the query does not match with the found rule
  this returns false. If it matches it goes further down to evaluate the rule."
  [factMap ruleConditions ruleValues]
  (def inValues (get ruleConditions 0))
  (if (not= (count ruleValues) inValues)
    false
    (evaluateFact factMap ruleConditions ruleValues)
    )
  )

(defn evaluateQueryRule
  "Second level of query processing. As there was no fact match we evaluate if
  a rule matches with the query (by name at this instance). If there is no rule
  by name this returns false, if there is it goes down another level"
  [query factMap ruleMap]
  (def ruleName (get (str/split query #"\(") 0))
  (def ruleValues (obtainValuesFromBrackets query))
  (if (contains? ruleMap ruleName)
    (evaluateFactsOnRule factMap (get ruleMap ruleName) ruleValues)
    false
    )
  )

(defn processQuery
  "First level of query processing. Returns true or false, depending on the query result.
  If the match is given by a fact, this will stop evaluating at this level."
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