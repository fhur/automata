(ns automata.core)

(def id-generator (let [counter (ref 0)]
                    (fn [] (dosync (let [cur-val @counter]
                      (do (alter counter + 1)
                          cur-val))))))

(defn in?
  "true if seq contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn transition
  "Returns a list of states that are the result of applying
  the transition function on the given state and input"
  [nfa state input]
  (get (:transitions nfa) [state input] []))

(defn e-clos
  "A list of all states including the given state that are
  accesible via epsilon moves"
  [nfa state]
  (map e-clos (transition nfa state :eps)))

(defn accepting-inputs
  "Returns the list of inputs that a list of states accepts
  for a given NFA"
  [nfa states]
  (reduce (fn [inputs [in-state input _]]
            (if (in? states in-state)
              (conj inputs input)
              inputs))
          #{} (:transition-list nfa)))

(defn make-unique [keyw]
  (keyword (str (name keyw) (id-generator))))

(defn unique-transitions [transitions]
  (reduce (fn [red cur]
            (assoc red cur (make-unique cur)))
          {} (set (concat (map first transitions)
                          (map last transitions)))))

(defn concat-transition-lists
  [nfa1 nfa2]
  (concat (:transition-list nfa1)
          (:transition-list nfa2)))

(defn- create-nfa-raw
  "Creates an NFA"
  [init end transitions]
  {:init init
   :end end
   :transition-list transitions
   :transitions (reduce (fn [transition-map [in-state input out-state]]
                          (assoc transition-map
                                 [in-state input]
                                 (conj (get transition-map [in-state input] []) out-state)))
                        {} transitions)})

(defn create-nfa
  [init end & transitions]
  (let [unique-map (unique-transitions transitions)
        unique! (fn [state] (get unique-map state))]
    (create-nfa-raw (unique! init)
                    (unique! end)
                    (map (fn [[in-state input out-state]]
                           (vector (unique! in-state)
                                   input
                                   (unique! out-state)))
                         transitions))))

(defn single-char-nfa [c]
  (create-nfa :init :end [:init c :end]))

(defn nfa-cat [nfa1 nfa2]
  (apply create-nfa (:init nfa1) (:end nfa2)
         (conj (concat-transition-lists nfa1 nfa2)
               [(:end nfa1) :eps (:init nfa2)])))

(defn nfa-or [nfa1 nfa2]
  (apply create-nfa :or-init :or-end
         (conj (concat-transition-lists nfa1 nfa2)
               [:or-init :eps (:init nfa1)]
               [:or-init :eps (:init nfa2)]
               [(:end nfa1) :eps :or-end]
               [(:end nfa2) :eps :or-end])))
