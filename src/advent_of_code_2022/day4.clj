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

(defn part1 [lines]
  (->> lines
       (map parse-line)
       (map fully-overlaps)
       (map #(if % 1 0))
       (reduce +)))

(println (read-input "day4.txt" part1))

(defn any-overlap [assignments]
  (let [[first second] assignments]
    (not-empty (set/intersection first second))))

(defn part2 [lines]
  (->> lines
       (map parse-line)
       (map any-overlap)
       (map #(if % 1 0))
       (reduce +)))

(println (read-input "day4.txt" part2))