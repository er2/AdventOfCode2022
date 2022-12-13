(ns advent-of-code-2022.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [fname]
  (str/split-lines (slurp (io/resource fname))))

(defn parse-line [line]
  (if (= "noop" line)
    :noop
    (Integer/parseInt (second (str/split line #" ")))))

(def init-state
  [{:cycle 1, :total 1}])

(defn process-instruction [states instr]
  (let [{cycle :cycle
         total :total} (last states)]
    (if (= :noop instr)
      (conj states
            {:cycle (inc cycle) :total total})
      (conj states
            {:cycle (inc cycle) :total total}
            {:cycle (inc (inc cycle)) :total (+ instr total)}))))

(defn run [lines]
  (->> lines
       (map parse-line)
       (reduce process-instruction init-state)))

(def states (run (read-input "day10.txt")))

(println (reduce process-instruction init-state [15 -11]))

(doseq [e (into (sorted-map) (zipmap (range 1 (count states)) states))]
  (println e))

(defn signal-strength [index item]
  (let [i (inc index)]
    (if (= 0 (mod i 20))
      (* item i))))

(defn keep-odds [index item]
  (if (= 0 (mod index 2))
    item))

(->> states
     (map :total)
     (keep-indexed signal-strength)
     (keep-indexed keep-odds)
     (reduce +)
     (println))

(doseq [state states
        :let [{cycle :cycle
               value :total} state
              x (mod (dec cycle) 40)
              lit (#{(inc x) x (dec x)} value)]]
  (print (if lit \█ \space))
  (if (= 39 x) (println)))