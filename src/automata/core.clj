(ns automata.core)

(def id-generator (let [counter (ref 0)]
                    (fn [] (dosync (let [cur-val @counter]
                      (do (alter counter + 1)
                          cur-val))))))

(defn conj-all
  "Conjoins all the items in coll 2 to coll"
  [coll coll2]
  (if (empty? coll2)
    coll
    (apply (partial conj coll) coll2)))

(defn in?
  "true if seq contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn transition
  "Returns a list of states that are the result of applying
  the transition function on the given state and input"
  [nfa state input]
  (get (:transitions nfa) [state input] []))

(defn transition*
  [nfa states input]
  (mapcat #(transition nfa % input) states))

(defn e-clos
  "A list of all states including the given state that are
  accesible via epsilon moves"
  [nfa state]
  (cons state
        (mapcat (partial e-clos nfa) (transition nfa state :eps))))

(defn e-clos-transition
  [nfa states input]
  (mapcat (partial e-clos nfa)
          (transition* nfa states input)))

(defn accepting-inputs
  "Returns the list of inputs that a list of states accepts
  for a given NFA"
  [nfa states]
  (reduce (fn [inputs [in-state input _]]
            (if (and (in? states in-state) (not= :eps input))
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

(defn nfa-cat
  ([nfa1 nfa2]
   (apply create-nfa (:init nfa1) (:end nfa2)
          (conj (concat-transition-lists nfa1 nfa2)
                [(:end nfa1) :eps (:init nfa2)])))
  ([nfa1 nfa2 & nfas]
   (reduce nfa-cat (nfa-cat nfa1 nfa2) nfas)))

(defn nfa-or
  ([nfa1 nfa2]
   (apply create-nfa :or-init :or-end
          (conj (concat-transition-lists nfa1 nfa2)
                [:or-init :eps (:init nfa1)]
                [:or-init :eps (:init nfa2)]
                [(:end nfa1) :eps :or-end]
                [(:end nfa2) :eps :or-end])))
  ([nfa1 nfa2 & nfas]
   (reduce nfa-or (nfa-or nfa1 nfa2) nfas)))

(defn nfa-?
  [nfa]
  (apply create-nfa :?-init :?-end
         (conj (:transition-list nfa)
               [:?-init :eps (:init nfa)]
               [:?-init :eps :?-end]
               [(:end nfa) :eps :?-end])))

(defn nfa-*
  [nfa]
  (apply create-nfa :kleen-init :kleen-end
         (conj (:transition-list nfa)
               [:kleen-init :eps :kleen-end]
               [:kleen-init :eps (:init nfa)]
               [(:end nfa) :eps :kleen-init])))

(defn nfa-+
  [nfa]
  (apply create-nfa :kleen-init :kleen-end
         (conj (:transition-list nfa)
               [:kleen-init :eps (:init nfa)]
               [(:end nfa) :eps :kleen-init]
               [(:end nfa) :eps :kleen-end])))

(defn dfa-init [nfa]
  (e-clos nfa (:init nfa)))

(defn dfa-end [nfa dfa-transitions]
  (reduce (fn [end-states [_ _ out-state]]
            (if (in? out-state (:end nfa))
              (conj end-states out-state)
              end-states))
          #{} dfa-transitions))

(defn- dfa-transitions
  "Step function in the process of converting an NFA to a DFA."
  [nfa states]
  (map (fn [input]
         [states input
          (e-clos-transition nfa states input)])
       (accepting-inputs nfa states)))

(defn new-transitions
  "Given a set of transitions a.k.a. transitions-set and a
  collection of transition this method will return all
  transitions that are in next-trns and not in transitions-set"
  [transitions-set next-trns]
  (clojure.set/difference (set next-trns) transitions-set))

(defn transitions->out-states
  "Given a list of transitions, this method returns the
  out state of each transition. Every transition is a
  [in-state input out-state] triple"
  [transitions]
  (set (map last transitions)))

(defn transitions:nfa->dfa
  [nfa]
  (loop [queue [(dfa-init nfa)]
         transitions #{}]
    (if (empty? queue)
      transitions
      (let [next-trns (dfa-transitions nfa (first queue))
            diff-trns (new-transitions transitions next-trns)]
        (recur (conj-all (rest queue)
                         (transitions->out-states diff-trns))
               (conj-all transitions next-trns))))))

(defn nfa->dfa
  [nfa]
  (let [transitions (transitions:nfa->dfa nfa)]
    {:init (dfa-init nfa)
     :end (dfa-end nfa transitions)
     :transition-list transitions
     :transitions (reduce (fn [trns [in state out]]
                            (assoc trns [in state] out))
                          {} transitions)}))

(defn exec-dfa
  [dfa string]
  (loop [string string
         state (:init dfa)]
    (cond
      (and (in? (:end dfa) state) (empty? string)) true
      (or (empty? string) (nil? state)) false
      :else (recur (.substring string 1)
                   (get (:transitions dfa) [state (.substring string 0 1)])))))
