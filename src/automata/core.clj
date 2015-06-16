(ns automata.core)

(defn create-dfa
  "Creates a hashmap based representation of a dfa
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

(defn create-nfa
  [init-state accept-states & transitions]
  {:init init-state
   :accept (set accept-states)
   :transitions (reduce (fn [nfa-map current]
                          (let [[from input to] current
                                trans-on-input (get nfa-map [from input] [])]
                            (assoc nfa-map [from input] (conj trans-on-input to))))
                        {} transitions)})

(defn get-transition
  [dfa state input]
  (if (nil? state)
    nil
    (get (:transitions dfa)
      [state (str input)])))

(defn get-transitions
  [nfa state input]
  (get (:transitions nfa)
       [state (if (= :eps input)
                :eps
                (str input))]
       []))


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
                (get-transition dfa state input))
            (:init dfa)
            (seq string))))

(defn eval-nfa
  ([nfa inputs state]
   (if (empty? inputs)
     [state]
     (let [input (first inputs)
           next-states (get-transitions nfa state input)
           eps-states  (get-transitions nfa state :eps)]
       (apply concat (concat (map #(eval-nfa nfa (rest inputs) %) next-states)
                             (map #(eval-nfa nfa inputs %) eps-states))))))
  ([nfa string]
   (reduce #(or %1 %2)
          (map #(contains? (:accept nfa) %)
               (eval-nfa nfa (seq string) (:init nfa))))))


