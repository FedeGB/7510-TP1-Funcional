(ns logical-interpreter)
(require '[clojure.string :as str])

(def factMap {})
(def ruleMap {})

(defn isValidRule
  [entry]
  (not (nil? (re-find #"^[^\(]*\([^)]*\) :- ([^\(]*\([^)]*\), )+([^\(]*\([^)]*\))$" entry)))
)

(defn isValidFact
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

(defn processValidRule
  [rule ruleMap]
  (def separated (str/split (str/replace rule #" " "") #":-"))
  (def ruleSide (get separated 0))
  (def factSide (get separated 1))
  (def ruleName (get (str/split ruleSide #"\(") 0))
  (def variables (str/split (str/replace (get (str/split ruleSide #"\(") 1) #"\)" "") #","))
  (def processedFacts (map putTogether (str/split factSide #"\) *,")))
  (def ruleList [(str (count variables))])
  (def ruleList (concat ruleList variables))
  (def ruleList (concat ruleList processedFacts))
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

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (def inputParsed (str/split (str/replace parent-database #"(\t|\n)" ".") #"\.+"))
  (def validInput true)
  (for [element inputParsed]
    (def validInput (and (operateInputElement element factMap ruleMap) validInput))
  )
  (println factMap)
  (println ruleMap)
  (if validInput

  )
  nil
)