(ns advent-of-code-2022.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input
  [fname]
  (with-open [rdr (clojure.java.io/reader (io/resource fname))]
    (reduce conj [] (line-seq rdr))))

(defn plays
  []
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(defn wins
  []
  #{
    [:rock :scissors]
    [:paper :rock]
    [:scissors :paper]
    })

(defn shape-score
  []
  {:rock 1
   :paper 2
   :scissors 3})

(defn outcome-score
  [game]
  (let [{opp :opp
         self :self} game
        opp-play ((plays) opp)
        self-play ((plays) self)]
    (cond
      (= opp-play self-play) 3
      ((wins) [self-play opp-play]) 6
      :else 0)))

(defn score
  [line]
  (let [[opp self] (str/split line #" ")
        game {:opp opp :self self}]
    (+
      ((shape-score) ((plays) self))
      (outcome-score game))))

(println ((shape-score) "A"))

(println (score "A Y"))

(println (reduce + (map score (read-input "day2.txt"))))
(println (reduce + (map score ["A Y" "B X" "C Z"])))
(println (reduce + (map score ["A Y"])))
(println (reduce + (map score ["B X"])))
(println (reduce + (map score ["C Z"])))

