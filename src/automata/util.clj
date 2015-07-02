(ns automata.util)

(def id-generator (let [counter (ref 0)]
                    (fn [] (dosync (let [cur-val @counter]
                      (do (alter counter + 1)
                          cur-val))))))

(defn conj-all
  "Conjoins all the items in coll2 to coll."
  [coll coll2]
  (if (or (nil? coll2) (empty? coll2))
    coll
    (apply (partial conj coll) coll2)))

(defn in?
  "true if seq contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn make-unique! [keyw]
  "Takes a keyword as argument and returns a unique keyword prefixed by that
  keyword. Every call to make-unique! is guaranteed to return a different keyword."
  (keyword (str (name keyw) (id-generator))))


