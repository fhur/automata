(ns automata.core-test
  (:require [clojure.test :refer :all]
            [automata.core :refer :all]
            [presto.core :refer :all]))

(def regex-simple-cat (regex . "foobar"))
(def regex-compound-cat (regex . "ab" "cd" "ef" "gh"))
(def regex-simple-kleen (regex * "01"))
(def regex-compound-kleen (regex * (. "0" "1" "2")))

(expected-when "simple concatenation" (partial exec-dfa regex-simple-cat)
  :when ["foobar"] = true
  :when ["f"] = false
  :when ["foo"] = false
  :when ["foob"] = false
  :when ["fooba"] = false
  :when ["foobarr"] = false)

(expected-when "compound concatenation" (partial exec-dfa regex-compound-cat)
  :when ["abcdefgh"] = true
  :when ["abcdefghi"] = false
  :when ["asdfasdf"] = false
  :when [""] = false
  :when ["abcgh"] = false
  :when ["abdefh"] = false
  :when ["abcdegh"] = false
  :when ["abcdefg"] = false)

(expected-when "simple kleen star" (partial exec-dfa regex-simple-kleen)
  :when [""] = true
  :when ["01"] = true
  :when ["0101"] = true
  :when ["01010101"] = true
  :when [" "] = false
  :when ["010"] = false
  :when ["0100"] = false
  :when ["00101"] = false)

#_(expected-when "compound kleen" (partial exec-dfa regex-compound-kleen)
  :when [""] = true
  :when ["0"] = true
  :when ["1"] = true
  :when ["2"] = true
  :when ["00"] = true
  :when ["01"] = true
  :when ["02"] = true
  :when ["10"] = true
  :when ["11"] = true
  :when ["12"] = true
  :when ["20"] = true
  :when ["21"] = true
  :when ["22"] = true
  :when ["10010020012121201201212"] = true
  :when [" 10010020012121201201212"] = false
  :when ["031213"] = false)






