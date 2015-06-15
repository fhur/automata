(ns automata.core-test
  (:require [clojure.test :refer :all]
            [automata.core :refer :all]
            [presto.core :refer :all]))


;; A dfa that matches 01..10 strings
(def dfa-01*0 (create-dfa :a [:c]
                [:a "0" :b]
                [:b "1" :b]
                [:b "0" :c]))

(expected-when "test-dfa-01*0" (partial eval-dfa dfa-01*0)
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
