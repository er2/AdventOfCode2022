(ns advent-of-code-2022.day2-2
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn read-input
  [fname]
  (with-open [rdr (clojure.java.io/reader (io/resource fname))]
    (reduce conj [] (line-seq rdr))))

(def plays
  {"A" :rock
   "B" :paper
   "C" :scissors})

(def outcome
  {"X" :lose
   "Y" :draw
   "Z" :win})

(def shape-score
  {:rock     1
   :paper    2
   :scissors 3})

(def outcome-score
  {:win  6
   :draw 3
   :lose 0})

(def losses
  {:rock     :scissors
   :scissors :paper
   :paper    :rock})

(def wins (set/map-invert losses))

(defn determine-my-play
  [opp-play outcome]
  (condp = outcome
    :draw opp-play
    :lose (losses opp-play)
    :win (wins opp-play)))

(defn score
  [line]
  (let [[opp outcome-char] (str/split line #" ")
        opp-play (plays opp)
        outcome (outcome outcome-char)
        self-play (determine-my-play opp-play outcome)]
    (+
      (shape-score self-play)
      (outcome-score outcome))))

(println (score "A Y"))


(println (reduce + (map score (read-input "day2.txt"))))
(println (reduce + (map score ["A Y" "B X" "C Z"])))
(println (reduce + (map score ["A Y"])))
(println (reduce + (map score ["B X"])))
(println (reduce + (map score ["C Z"])))

