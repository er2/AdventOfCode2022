(ns advent-of-code-2022.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn elf-sum
  [s]
  (reduce + (map
     #(Integer/parseInt %)
     (str/split s #"\n"))))

(defn single-max
  [elves]
  (reduce max elves))

(defn reverse-cmp [a b]
  (compare b a))

(defn sort-desc
  [list]
  (sort reverse-cmp list))

(defn top-3
  [elves]
  (reduce + (take 3 (sort-desc elves))))

(defn greatest-3+1
  [list fourth]
  (if (> (nth list 2) fourth)
    list
    (sort-desc (conj (take 2 list) fourth))))

(defn max3
  [list fourth]
  (if (> (nth list 2) fourth)
    list
    (take 3 (sort-desc (conj list fourth)))))

(defn top-3-smart
  [list]
  (let [first-3 (sort-desc (take 3 list))
        rest    (drop 3 list)]
    (reduce + (reduce max3 first-3 rest))))

(defn read-input
  [fname list-extract]
  (let [file-contents (slurp (io/resource fname))
        by-elf        (str/split file-contents #"\n\n")
        sums-by-elf   (map elf-sum by-elf)]
    (list-extract sums-by-elf)))

(println (greatest-3+1 [4 3 2] 1))
(println (greatest-3+1 [3 2 1] 4))

(println (top-3-smart '(2 2 2 1)))

(println (sort-desc [1 2 3 4]))
(println (sort-desc [5 4 3 2 1]))
(println (sort-desc [9 5 2 8]))

(println "part 1" (read-input "day1.txt" single-max))
(println "part 2" (read-input "day1.txt" top-3))
(println "part 2" (read-input "day1.txt" top-3-smart))
