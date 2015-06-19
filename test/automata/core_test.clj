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

(def nfa-empty-string (create-nfa :a [:b] [:a :eps :b]))

;; A NFA for the "" regex
(expected-when "Empty string NFA should only accept the empty string" (partial eval-nfa nfa-empty-string)
  :when [""] = true
  :when [" "] = false
  :when ["a"] = false
  :when ["ba"] = false
  :when ["c"] = false
  :when ["dasdf"] = false
  :when ["1234"] = false)

(let [single-char-nfa (create-nfa :a [:b] [:a "a" :b])]
  (expected-when "A NFA that accepts only the 'a' string" (partial eval-nfa single-char-nfa)
    :when [""] = false
    :when [" "] = false
    :when ["a"] = true
    :when ["ab"] = false
    :when ["a "] = false
    :when ["ba"] = false
    :when ["b"] = false))

(let [single-char-concat-nfa (create-nfa :a [:end] [:a "a" :b]
                                                   [:b "b" :c]
                                                   [:c "c" :end])
      exec-nfa (partial eval-nfa single-char-concat-nfa)]
  (expected-when "An NFA that accepts the 'abc' concat" exec-nfa
    :when [""] = false
    :when [" "] = false
    :when ["a"] = false
    :when ["ab"] = false
    :when ["abc"] = true
    :when ["abcd"] = false))

(let [or-nfa (create-nfa :a [:end] [:a :eps :b] [:a :eps :c]
                                   [:b "1" :d]
                                   [:c "0" :e]
                                   [:d :eps :end]
                                   [:e :eps :end])
      exec-nfa (partial eval-nfa or-nfa)]
  (expected-when "An NFA that accepts either 0 or 1 but not both" exec-nfa
    :when [""] = false
    :when ["0"] = true
    :when ["1"] = true
    :when [" "] = false
    :when ["10"] = false
    :when ["01"] = false))

(let [a-nfa (single-char-nfa "a")
      b-nfa (single-char-nfa "b")
      c-nfa (single-char-nfa "c")
      eval-nfa-xor #(eval-nfa (apply nfa-xor %1) %2)
      eval-nfa-cat #(eval-nfa (apply nfa-cat %1) %2)
      eval-nfa-kleen #(eval-nfa (nfa-kleen %1) %2)
      eval-nfa-opt #(eval-nfa (nfa-optional %1) %2)]

  (expected-when "nfa-xor should allow one or the other but not both" eval-nfa-xor
    :when [[a-nfa b-nfa] "ab"] = false
    :when [[a-nfa b-nfa] "a"] = true
    :when [[a-nfa b-nfa] "b"] = true
    :when [[a-nfa c-nfa] "c"] = true
    :when [[a-nfa c-nfa] "a"] = true
    :when [[a-nfa b-nfa c-nfa ] "a"] = true
    :when [[a-nfa b-nfa c-nfa ] "b"] = true
    :when [[a-nfa b-nfa c-nfa ] "c"] = true
    :when [[a-nfa b-nfa c-nfa ] "ca"] = false
    :when [[a-nfa b-nfa c-nfa ] "cb"] = false
    :when [[a-nfa b-nfa c-nfa ] "ac"] = false
    :when [[a-nfa b-nfa c-nfa ] "bc"] = false
    :when [[a-nfa b-nfa c-nfa ] "bca"] = false
    :when [[a-nfa b-nfa c-nfa ] ""] = false)

  (expected-when "nfa-cat should allow only serial concatenations of strings" eval-nfa-cat
    :when [[a-nfa b-nfa] "ab"] = true
    :when [[b-nfa b-nfa] "bb"] = true
    :when [[b-nfa a-nfa] "ba"] = true
    :when [[b-nfa b-nfa] "bb"] = true
    :when [[a-nfa b-nfa c-nfa] "abc"] = true
    :when [[a-nfa b-nfa b-nfa] "abb"] = true
    :when [[a-nfa b-nfa a-nfa b-nfa] "abab"] = true
    :when [[a-nfa b-nfa] "abc"] = false
    :when [[a-nfa b-nfa] "abab"] = false)

  (expected-when "nfa-kleen should allow 0 or more repetitions of an nfa" eval-nfa-kleen
    :when [a-nfa ""] = true
    :when [a-nfa "a"] = true
    :when [a-nfa "aa"] = true
    :when [a-nfa "aaa"] = true
    :when [a-nfa "aaaa"] = true
    :when [a-nfa "aaaaaaaaaaaaa"] = true
    :when [a-nfa "b"] = false
    :when [a-nfa "ab"] = false
    :when [a-nfa "aab"] = false
    :when [a-nfa "baabaa"] = false
    :when [(nfa-cat a-nfa b-nfa) ""] = true
    :when [(nfa-cat a-nfa b-nfa) "ab"] = true
    :when [(nfa-cat a-nfa b-nfa) "abab"] = true
    :when [(nfa-cat a-nfa b-nfa) "ababab"] = true
    :when [(nfa-cat a-nfa b-nfa) "ababab"] = true
    :when [(nfa-cat a-nfa b-nfa) "abababab"] = true
    :when [(nfa-cat a-nfa b-nfa) "abaabab"] = false
    :when [(nfa-cat a-nfa b-nfa) "abababb"] = false
    :when [(nfa-cat a-nfa b-nfa) "abababa"] = false
    :when [(nfa-cat a-nfa b-nfa) " "] = false
    :when [(nfa-cat a-nfa b-nfa) "aba"] = false
    :when [(nfa-xor a-nfa b-nfa) ""] = true
    :when [(nfa-xor a-nfa b-nfa) "a"] = true
    :when [(nfa-xor a-nfa b-nfa) "b"] = true
    :when [(nfa-xor a-nfa b-nfa) "ab"] = true
    :when [(nfa-xor a-nfa b-nfa) "baa"] = true
    :when [(nfa-xor a-nfa b-nfa) "abaaabbbaababababa"] = true)

  (expected-when "nfa-optional should allow 0 or 1 of an nfa" eval-nfa-opt
    :when [a-nfa ""] = true
    :when [a-nfa "a"] = true
    :when [a-nfa "b"] = false
    :when [(nfa-cat a-nfa b-nfa) "" ] = true
    :when [(nfa-cat a-nfa b-nfa) "ab" ] = true))


