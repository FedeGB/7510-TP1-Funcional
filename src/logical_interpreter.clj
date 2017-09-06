(ns logical-interpreter)
(require '[clojure.string :as str])

(def factMap {})
(def ruleMap {})

(defn isValidRule
  [entry]
  (not (nil? (re-find #"^[^\(]*\([^)]*\) :- [^\(]*\([^)]*\), [^\(]*\([^)]*\)$" entry)))
)

(defn isValidFact
  [entry]
  (not (nil? (re-find #"^[^\(]*\([^)]*\)$" entry)))
)

(defn processValidFact
  [fact factMap]
  (def allTogether (str/replace fact #"( *\( *| *\) *| *, *)" ""))
  (def factMap (assoc factMap  allTogether 1))
  nil
)

(defn processValidRule
  [rule ruleMap]

  nil
)

(defn operateInputElement
  [element factMap]
  (def case false)
  (if (isValidRule element)
    (processValidRule element ruleMap)
    (def case true)
  )
  (if (isValidFact element)
    (processValidFact element factMap)
    (def case true)
  )
  case
)

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (def inputParsed (str/split (str/replace parent-database #"(\t|\n)" "") #"\."))
  (def validInput true)
  (for [element inputParsed]
    (and (operateInputElement element factMap) validInput)
  )
  (println factMap)
  (if validInput

  )
  nil
)