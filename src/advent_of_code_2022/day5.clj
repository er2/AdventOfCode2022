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
  (->> (str/split instruction #" ")
       (partition 2)
       (map parse-inst)
       (into {})))

(defn parse-instructions [instructions]
  (map parse-instruction (str/split-lines instructions)))

(defn make-run [part-func]
  (fn [state instruction]
    (let [{move :move
           from :from
           to   :to} instruction
          froml (state from)
          [to-move leave] (split-at move froml)
          newl (concat (part-func to-move) (state to))]
      (-> state
          (assoc from leave)
          (assoc to newl)))))

(defn first-val [m k v]
  (assoc m k (first v)))

(defn exec [run-func]
  (let [[setup-str instructions-str] (read-input "day5.txt")
        setup (parse-setup setup-str)
        instructions (parse-instructions instructions-str)]
    (reduce run-func setup instructions)))

(def result1
  (exec (make-run reverse)))

(println result1)

(defn end-state-to-answer [state]
  (->> state
       (reduce-kv first-val {})
       (into (sorted-map))
       (vals)
       (apply str)))

(println (end-state-to-answer result1))

(def result2
  (exec (make-run identity)))

(println result2)

(println (end-state-to-answer result2))