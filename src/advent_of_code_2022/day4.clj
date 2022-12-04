(ns advent-of-code-2022.day4
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-assignment [assignment]
  (let [[lo hi] (map #(Integer/parseInt %) (str/split assignment #"-"))]
    (set (range lo (+ 1 hi)))))

(defn parse-line [line]
  (map parse-assignment (str/split line #",")))

(defn read-input [fname f]
  (with-open [rdr (clojure.java.io/reader (io/resource fname))]
    (f (line-seq rdr))))

(defn fully-overlaps [assignments]
  (let [[first second] assignments]
    (or (set/subset? first second)
        (set/subset? second first))))

(println (fully-overlaps (parse-line "2-4,6-8")))
(println (fully-overlaps (parse-line "2-4,2-5")))

(defn count-so [predicate vals]
  (->> vals
       (map #(if (predicate %) 1 0))
       (reduce +)))

(defn part1 [lines]
  (->> lines
       (map parse-line)
       (count-so fully-overlaps)))

(println (read-input "day4.txt" part1))

(defn any-overlap [assignments]
    (not-empty (apply set/intersection assignments)))

(defn part2 [lines]
  (->> lines
       (map parse-line)
       (count-so any-overlap)))

(println (read-input "day4.txt" part2))