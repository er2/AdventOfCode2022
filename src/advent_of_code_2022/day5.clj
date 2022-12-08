(ns advent-of-code-2022.day5
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(defn read-input
  [fname]
  (let [file-contents (slurp (io/resource fname))]
    (str/split file-contents #"\n\n")))

(defn str-count [s ch]
  ((frequencies s) ch))

(defn count-stacks [s]
  (let [penultimate-line (last (drop-last 1 (str/split-lines s)))]
    (str-count penultimate-line \[)))

(defn make-chat [lines]
  (fn [x y]
    (get (get lines y) x)))

(defn count-down [n]
  (range n -1 -1))

(defn valid-letter [ch]
  (and
    (not (nil? ch))
    (not (= ch \space))))

(defn map-merge [accum elem]
  (let [[k v] elem]
    (assoc accum k (filter valid-letter (cons v (accum k))))))

(defn parse-setup [s]
  (let [n-stacks (count-stacks s)
        lines (str/split-lines s)
        max-depth (- (count lines) 2)
        chat (make-chat lines)]
    (reduce map-merge
            {}
            (for [stack (range 1 (+ 1 n-stacks))
                  elem (count-down max-depth)]
              (let [x (+ 1 (* 4 (- stack 1)))]
                [stack (chat x elem)])))))

(defn parse-inst [kv]
  (let [[k v] kv]
    [(keyword k)
    (Integer/parseInt v)]))

(defn parse-instruction [instruction]
  (into {} (map parse-inst (partition 2 (str/split instruction #" ")))))

(defn parse-instructions [instructions]
  (map parse-instruction (str/split-lines instructions)))

;(println (parse-setup (get (read-input "day5.txt") 0)))
;(println (parse-instructions (get (read-input "day5.txt") 1)))

(let [[setup-str instructions-str] read-input "day5.txt"
      setup (parse-setup setup-str)
      instructions (parse-instructions instructions-str)]
  )

(def sample-data
  {
   1 [\Z \N]
   2 [\M \C \D]
   3 [\P]
   })