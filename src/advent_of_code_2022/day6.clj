(ns advent-of-code-2022.day6
  (:require [clojure.java.io :as io]))

(defn sliding-window-helper [n coll]
  (let [[first second third fourth & _] coll]
    (if (= 4 (count (set [first second third fourth])))
      (+ 4 n)
      (sliding-window-helper (+ n 1) (drop 1 coll)))))

(defn sliding-window [coll]
  (sliding-window-helper 0 coll))

(println (sliding-window "bvwbjplbgvbhsrlpgdmjqwftvncz"))
(println (sliding-window "nppdvjthqldpwncqszvftbrmjlhg"))
(println (sliding-window "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
(println (sliding-window "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

(defn read-input [fname f]
  (f (slurp (io/resource fname))))

(println (read-input "day6.txt" sliding-window))


(defn sliding-window-gen [k]
  (let [helper-fn (fn sliding-window-helper [n coll]
                    (if (= k (count (set (take k coll))))
                      (+ k n)
                      (sliding-window-helper (+ n 1) (drop 1 coll))))]
    (fn [coll]
      (helper-fn 0 coll))))

(println ((sliding-window-gen 14) "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
(println (read-input "day6.txt" (sliding-window-gen 14)))