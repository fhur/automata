(ns automata.core)

(defn create-dfa
  "Creates a hashmap based representation of a DFA.
  Syntax: (create-dfa init-state accept-states & transitions)
  Where each transition is a 3-tuple of ['initial state' 'input' 'resulting state']
  Note: this method does no validation on the transitions."
  [init-state accept-states & transitions ]
  {:pre [(coll? accept-states)
         (not (empty? transitions))
         (not (empty? accept-states))]}
  {:init init-state
   :accept (set accept-states)
   :transitions (reduce (fn [dfa-map current]
                          (let [[from input to] current]
                            (assoc dfa-map [from input] to)))
                        {} transitions)})

(defn accept?
  "Returns true if the given state is an accepting state for the dfa"
  [dfa state]
  (contains? (:accept dfa)
             state))

(defn eval-dfa
  "Executes the given DFA as created by create-dfa using the given
  string as input. The method returns true if both the string finished
  and the automata resulted in an accepting state"
  [dfa string]
  (accept? dfa
    (reduce (fn [state input]
              (if (nil? state)
                nil
                (get (:transitions dfa)
                     [state (str input)])))
            (:init dfa)
            (seq string))))





