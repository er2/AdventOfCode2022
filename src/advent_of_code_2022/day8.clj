(ns advent-of-code-2022.day8
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(defn read-input [fname]
  (slurp (io/resource fname)))

(defn make-height-at [lines]
  (fn height-at-coord [[x y]]
    (-> lines
        (get y)
        (get x)
        (str)
        (Integer/parseInt))))

(defn height-map [lines max-x max-y]
  (let [height-at (make-height-at lines)]
    (into {} (for [y (range max-y)
                   x (range max-x)]
               [[x y] (height-at [x y])]))))

(defn parse-forest [file-contents]
  (let [lines (str/split-lines file-contents)
        max-x (count (first lines))
        max-y (count lines)]
    {:max-x     max-x
     :max-y     max-y
     :height-at (height-map lines max-x max-y)}))

(defn count-down [x]
  "counts from x (exclusive) down to 0 (inclusive)"
  (range (dec x) -1 -1))

(defn make-get-potential-blockers [max-x max-y]
  (fn get-potential-blockers [x y]
    {:w (map (fn [x] [x y]) (count-down x))
     :e (map (fn [x] [x y]) (range (inc x) max-x))
     :n (map (fn [y] [x y]) (count-down y))
     :s (map (fn [y] [x y]) (range (inc y) max-y))}))

(defn map-values [m v-func]
  (reduce-kv (fn [m k v] (assoc m k (v-func v))) {} m))

(defn blocker-heights [blockers height-at]
  (map-values blockers #(map height-at %)))

(defn blocked-by [height heights-in-one-direction]
  (some? (some #(<= height %) heights-in-one-direction)))

(defn blocked? [height potential-blocker-heights]
  (let [blocks #(blocked-by height %)]
    (->> potential-blocker-heights
         (vals)
         (map blocks)
         (every? true?))))

(defn count-true [coll]
  (count (filter true? coll)))

(defn count-visible-trees [{:keys [max-x max-y height-at]}]
  (let [get-potential-blockers (make-get-potential-blockers max-x max-y)]
    (count-true
      (for [y (range max-y)
            x (range max-x)
            :let [height (height-at [x y])
                  potential-blockers (get-potential-blockers x y)
                  potential-blocker-heights (blocker-heights potential-blockers height-at)
                  is-blocked (blocked? height potential-blocker-heights)]]
        (not is-blocked)))))

(->> (read-input "day8.txt")
     (parse-forest)
     (count-visible-trees)
     (println "Part 1:"))

(defn visible-in-one-direction [height trees]
  (reduce (fn [accum elem]
            (if (<= height elem)
              (reduced (inc accum))
              (inc accum)))
          0 trees))

(defn visible-in-all-directions [height potential-blockers]
  (reduce * (map #(visible-in-one-direction height %) (vals potential-blockers))))

(defn count-visible-trees [{:keys [max-x max-y height-at]}]
  (let [get-potential-blockers (make-get-potential-blockers max-x max-y)]
    (reduce max
            (for [y (range max-y)
                  x (range max-x)
                  :let [height (height-at [x y])
                        potential-blockers (get-potential-blockers x y)
                        potential-blocker-heights (blocker-heights potential-blockers height-at)]]
              (visible-in-all-directions height potential-blocker-heights)))))

(->> (read-input "day8.txt")
     (parse-forest)
     (count-visible-trees)
     (println "Part 2:"))