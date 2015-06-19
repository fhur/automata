(ns automata.core)


(defn- create-nfa-raw
  [init-state accept-states transitions]
  {:init init-state
   :accept (set accept-states)
   :transitions (reduce (fn [nfa-map current]
                          (let [[from input to] current
                                trans-on-input (get nfa-map [from input] [])]
                            (assoc nfa-map [from input] (conj trans-on-input to))))
                        {} transitions)})

(defn gensym-states
  [transitions]
  (reduce (fn [m state] (assoc m state (gensym state)))
          {} (set (concat (map first transitions) (map last transitions)))))

(defn create-dfa
  "Creates a hashmap based representation of a DFA
  Syntax: (create-dfa init-state accept-states & transitions)
  Where each transition is a 3-tuple of ['initial state' 'input' 'resulting state']
  Note: this method does no validation on the transitions."
  [init-state accept-states & transitions ]
  {:pre [(coll? accept-states)
         (not (empty? transitions))
         (not (empty? accept-states))]}
  {:init init-state
   :states (set (concat (map first transitions) (map last transitions)))
   :accept (set accept-states)
   :transitions (reduce (fn [dfa-map current]
                          (let [[from input to] current]
                            (assoc dfa-map [from input] to)))
                        {} transitions)})

(defn create-nfa
  "Creates a hashmap based representation of a NFA
  each transition is a 3-tuple of ['state' 'input' 'resulting state']
  Epsilon transitions can be specified as ['state' :eps 'resulting state']"
  [init-state accept-states & transitions]
  (let [sym-map (gensym-states transitions)]
    (create-nfa-raw (get sym-map init-state)
                    (map (partial get sym-map) accept-states)
                    (map (fn [trns]
                           (vector (get sym-map (first trns))
                                   (second trns)
                                   (get sym-map (last trns)))) transitions))))

(defn nfa-transitions-list
  "Obtains a list of transitions [state input end-state] given an nfa"
  [nfa]
  (mapcat (fn [key-val]
            (let [[k vs] key-val]
              (map #(conj k %) vs)))
          (:transitions nfa)))

(defn in?
  "Return true if setcoll contains at least one element in values"
  [setcoll values]
  {:pre [(set? setcoll)]}
  (not (nil? (some setcoll values))))

(defn- get-transition
  [dfa state input]
  (if (nil? state)
    nil
    (get (:transitions dfa)
      [state (str input)])))

(defn- merge-repeated
  [coll item-to-repeat]
  (map vector coll (repeat item-to-repeat)))

(defn get-transitions
  [nfa state input]
  (let [get-next #(merge-repeated (get (:transitions nfa) [state %1] []) %2)]
      (concat (get-next :eps 0)
              (get-next (str input) 1))))

(defn- accept?
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
  "Executed the given NFA as created by create-nfa using the given string."
  [nfa string]
  ;; Create a queue that will hold all calls that must be processed recursively
  (loop [queue [[(seq string) (:init nfa)]]
         result []]
    (if (empty? queue)
      ;; once the queue is empty, we have finished processing everything and so
      ;; we can just return the result
      (in? (:accept nfa) result)
      (let [[charseq state] (first queue) ;; take the first from the queue
            input (first charseq) ;; the the first char of input
            next-states (get-transitions nfa state input)
            next-res (if (empty? charseq) (conj result state) result)]
        (recur (concat (rest queue) ;; pop one from the queue
                       (map #(vector (drop (second %) charseq) (first %))
                            next-states))
               next-res)))))

(defn nfa-cat
  "Concatenates several NFAs, equivalent to regex
  concatenations. This assumes that the NFAs have only
  one termination state."
  ([nfa1 nfa2]
   (create-nfa-raw (:init nfa1) (:accept nfa2)
                   (conj (mapcat nfa-transitions-list [nfa1 nfa2])
                         (vector (first (:accept nfa1)) :eps (:init nfa2)))))
  ([nfa1 nfa2 & nfas]
   (reduce nfa-cat (nfa-cat nfa1 nfa2) nfas)))

(defn nfa-xor
  ([nfa1 nfa2]
  (let [init (gensym :or-init)
        end  (gensym :or-end)
        end-nfa1 (first (:accept nfa1))
        end-nfa2 (first (:accept nfa2))]
    (create-nfa-raw init [end]
                    (conj (mapcat nfa-transitions-list [nfa1 nfa2])
                          [init :eps (:init nfa1)]
                          [init :eps (:init nfa2)]
                          [end-nfa1 :eps end]
                          [end-nfa2 :eps end]))))
  ([nfa1 nfa2 & nfas]
   (reduce nfa-xor (nfa-xor nfa1 nfa2) nfas)))


(defn nfa-kleen
  [nfa]
  (let [init (gensym :kleen-init)
        end  (gensym :kleen-end)
        nfa-end (first (:accept nfa))]
    (create-nfa-raw init [end]
                    (conj (nfa-transitions-list nfa)
                          [init :eps (:init nfa)]
                          [init :eps end]
                          [nfa-end :eps init]))))


(defn single-char-nfa
  [ch]
  (create-nfa :a [:end] [:a ch :end]))
