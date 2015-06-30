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
  (cons state
        (mapcat (partial e-clos nfa) (transition nfa state :eps))))

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


