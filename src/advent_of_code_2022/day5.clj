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

(defn penultimate [coll]
  (last (drop-last 1 coll)))

(defn count-stacks [lines]
  (str-count (penultimate lines) \[))

(defn make-char-at [lines]
  (fn [x y]
    (get (get lines y) x)))

(defn count-down [n]
  (range n -1 -1))

(defn valid-letter [ch]
  (and
    (some? ch)
    (not= ch \space)))

(defn map-merge [accum elem]
  (let [[k v] elem
        newvalue (filter valid-letter (cons v (accum k)))]
    (assoc accum k newvalue)))

(defn to-string-loc [stack-number]
  (inc (* 4 (dec stack-number))))

(defn make-lookup [setup-lines]
  (let [char-at (make-char-at setup-lines)]
    (fn lookup [stack-number element-number]
      (let [x (to-string-loc stack-number)]
        (char-at x element-number)))))

(defn parse-setup [s]
  (let [lines (str/split-lines s)
        n-stacks (count-stacks lines)
        max-depth (- (count lines) 2)
        lookup (make-lookup lines)]
    (reduce map-merge
            {}
            (for [stack (range 1 (inc n-stacks))
                  elem (count-down max-depth)]
              [stack (lookup stack elem)]))))

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