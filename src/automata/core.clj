(ns automata.core)

(defn create-dfa
  [init-state accept-states & transitions ]
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
  [dfa string]
  (reduce (fn [state input]
            (if (nil? state)
              nil
              (get (:transitions dfa) [state (str input)])))
          (:init dfa)
          (seq string)))

(def dfa-01*0 (create-dfa :a [:c]
                          [:a "0" :b]
                          [:b "1" :b]
                          [:b "0" :c]))





