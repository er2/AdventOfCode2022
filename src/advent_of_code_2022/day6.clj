(ns advent-of-code-2022.day6
  (:require [clojure.java.io :as io]))

(defn read-input [fname f]
  (f (slurp (io/resource fname))))

(defn sliding-window-gen [k]
  (let [helper-fn (fn sliding-window-helper [n coll]
                    (if (= k (count (set (take k coll))))
                      (+ k n)
                      (sliding-window-helper (+ n 1) (drop 1 coll))))]
    (fn [coll]
      (helper-fn 0 coll))))

(def part1-fn
  (sliding-window-gen 4))

(println (read-input "day6.txt" part1-fn))

(def part2-fn
  (sliding-window-gen 14))

(println (read-input "day6.txt" part2-fn))