(ns automata.core-test
  (:require [clojure.test :refer :all]
            [automata.core :refer :all]
            [presto.core :refer :all]))


;; A dfa that matches 01..10 strings
(def dfa-01*0 (create-dfa :a [:c]
                [:a "0" :b]
                [:b "1" :b]
                [:b "0" :c]))

(expected-when "test dfa 01*0" (partial eval-dfa dfa-01*0)
  :when ["00"] = true
  :when ["010"] = true
  :when ["0110"] = true
  :when ["01110"] = true
  :when ["011110"] = true
  :when ["asd"] = false
  :when ["0"] = false
  :when ["000"] = false
  :when ["0101"] = false)

;; A dfa that matches foox*bar
(def dfa-2 (create-dfa :a [:g]
                       [:a "f" :b]
                       [:b "o" :c]
                       [:c "o" :d]
                       [:d "x" :d]
                       [:d "b" :e]
                       [:e "a" :f]
                       [:f "r" :g]))

(expected-when "test-dfa-2" (partial eval-dfa dfa-2)
  :when ["foobar"] = true
  :when ["fooxbar"] = true
  :when ["fooxxbar"] = true
  :when ["fooxxxbar"] = true
  :when ["fooxxxxbar"] = true
  :when ["fooxfoobar"] = false
  :when ["barxfoo"] = false
  :when [""] = false
  :when ["bi"] = false
  :when ["fooxba"] = false
  :when ["fooxb"] = false)

;; A DFA that accepts [01]*1*
(def dfa-3 (create-dfa :a [:c]
                       [:a "0" :b]
                       [:a "1" :c]
                       [:b "0" :b]
                       [:b "1" :c]
                       [:c "0" :b]
                       [:c "1" :c]))

(expected-when "test dfa-3 [01]*1*" (partial eval-dfa dfa-3)
  :when ["1"] = true
  :when ["001"] = true
  :when ["011"] = true
  :when ["0101"] = true
  :when ["11111"] = true
  :when ["001100111"] = true
  :when ["011001"] = true
  :when ["0"] = false
  :when [""] = false
  :when ["00"] = false
  :when ["10"] = false
  :when ["111110"] = false)

;; A NFA equivalent to dfa-3 i.e. [01]*1*
(def nfa-1 (create-nfa :a [:j]
                       [:a :eps :b]
                       [:a :eps :h]
                       [:b :eps :c]
                       [:b :eps :d]
                       [:c "1" :e]
                       [:d "0" :f]
                       [:e :eps :g]
                       [:f :eps :g]
                       [:g :eps :a]
                       [:g :eps :h]
                       [:h :eps :i]
                       [:i "1" :j]))

(expected-when "transitions should be group by state input" (partial get (:transitions nfa-1))
  :when [[:a :eps]] = [:b :h]
  :when [[:b :eps]] = [:c :d]
  :when [[:g :eps]] = [:a :h])

(expected-when "NFA should accept [01]*1*" (partial eval-nfa nfa-1)
  :when ["1"] = true
  :when ["001"] = true
  :when ["011"] = true
  :when ["0101"] = true
  :when ["11111"] = true
  :when ["001100111"] = true
  :when ["011001"] = true
  :when ["0"] = false
  :when [""] = false
  :when ["00"] = false
  :when ["10"] = false
  :when ["111110"] = false
  :when ["1111000000000000000000000101010101011"] = true
  :when [ (apply str (repeat 500 "10101")) ] = true) ;; this tests that there is no stack overflow for large string


