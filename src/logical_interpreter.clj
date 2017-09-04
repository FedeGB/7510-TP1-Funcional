(ns logical-interpreter)
(require '[clojure.string :as str])

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
  (def factMap {})
  ;(assoc factMap {(get fact 0) (get fact 1)})

)

(defn operateElement
  [element]
  (if (isValidRule element)
    (def case (str/split element #"(:-)"))
  )
  (if (isValidFact element)
    (def case (str/split element #"(\(|\))"))
  )
  (if-not (or (isValidFact element) (isValidRule element))
    (def case nil)
  )
  case
)

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database query]
  (def inputParsed (str/split (str/replace parent-database #"(\t|\n)" "") #"\."))
  (for [element inputParsed]
    (println (operateElement element))
  )
  nil
)