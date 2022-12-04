(ns advent-of-code-2022.day3
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as str]))

(defn char-range [start end]
  (map char (range (int start) (+ 1 (int end)))))

(def priority-str
  (str (apply str (char-range \a \z)) (apply str (char-range \A \Z))))

(defn priority [ch]
  (+ 1 (str/index-of priority-str ch)))

(defn split-in-half [items]
  (let [midpoint (/ (count items) 2)]
    [(subs items 0 midpoint)
     (subs items midpoint)]))

(defn rucksack-to-compartments
  "takes a rucksack string and converts it to a vector of sets of items"
  [items]
  (map set (split-in-half items)))

(defn common-element [rucksack]
  (let [[compartment-a compartment-b] rucksack]
    (first (set/intersection compartment-a compartment-b))))

(println (apply str (char-range \a \z)))

(println (common-element [#{\a \b} #{\b \c}]))

(println (common-element (rucksack-to-compartments "abcb")))

(println (priority \Z))
(println priority-str)

(def sample
  "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw\n")

(defn read-input [fname f]
  (with-open [rdr (clojure.java.io/reader (io/resource fname))]
    (f (line-seq rdr))))

(defn part1 [lines]
  (->> lines
       (map rucksack-to-compartments)
       (map common-element)
       (map priority)
       (reduce +)))

(println (part1 (str/split-lines sample)))

(println (read-input "day3.txt" part1))

(defn badge [elves]
  (first (reduce set/intersection (map set elves))))

(def first-group
  "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg")

(def second-group
  "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw")

(println (badge (str/split-lines first-group)))
(println (badge (str/split-lines second-group)))

(defn part2 [lines]
  (->> lines
       (partition 3)
       (map badge)
       (map priority)
       (reduce +)))

(println (read-input "day3.txt" part2))