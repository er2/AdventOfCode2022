(ns advent-of-code-2022.day8
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(defn read-input [fname]
  (slurp (io/resource fname)))

(def file-contents (read-input "day8.txt"))

(defn make-height-at [lines]
  (fn height-at-coord [[x y]]
    (-> lines
        (get y)
        (get x)
        (str)
        (Integer/parseInt))))

(defn parse-forest [file-contents]
  (let [lines (str/split-lines file-contents)]
    {:max-x     (count (first lines))
     :max-y     (count lines)
     :height-at (make-height-at lines)}))

(defn make-get-potential-blockers [max-x max-y]
  (fn get-potential-blockers [x y]
    {:w (map (fn [x] [x y]) (range 0 x))
     :e (map (fn [x] [x y]) (range (inc x) max-x))
     :n (map (fn [y] [x y]) (range 0 y))
     :s (map (fn [y] [x y]) (range (inc y) max-y))}))

(defn map-values [m v-func]
  (reduce-kv (fn [m k v] (assoc m k (v-func v))) {} m))

(defn blocker-heights [blockers height-at]
  (map-values blockers height-at))

(defn blocked-by [height heights-in-one-direction]
  (some? (some #(<= height %) heights-in-one-direction)))

(defn blocked? [height potential-blocker-heights]
  (let [blocks #(blocked-by height %)]
    (-> potential-blocker-heights
        (map-values blocks)
        (map #{:n :s :e :w})
        (#(every? true? %)))))

(println "bl" (blocked-by 5 '(6)))
(println "bl" (blocked-by 5 '(6 4)))
(println "bl" (blocked-by 5 '(4)))
(println "bl" (blocked-by 5 '(5)))
(println "blocked" (blocked? 10 {:n '(9)}))
(println "blocked" (blocked? 10 {:n '(9) :s '(10)}))
(println "blocked" (blocked? 10 {:n '(10) :s '(10) :e '(10) :w '(10)}))

(def forest (parse-forest file-contents))

(def height-at (:height-at forest))

(def get-potential-blockers (make-get-potential-blockers 3 3))
(def blockers (get-potential-blockers 1 1))
(println "blockers" blockers)
;(println (map-values {:a 1 :b 2} inc))
;(println (+ 1 (height-at [0 0])))
;(println (height-at [1 1]))
;(println (height-at [1 0]))
;(println (height-at [0 1]))
;(println (height-at [2 1]))
(println (map-values blockers #(map height-at %)))

(defn count-visible-trees [{:keys [max-x max-y height-at]}]
  (let [get-potential-blockers (make-get-potential-blockers max-x max-y)]
    (count
      (filter :visible
              (for [y (range max-y)
                    x (range max-x)
                    :let [height (height-at [x y])
                          potential-blockers (get-potential-blockers x y)
                          potential-blocker-heights (blocker-heights potential-blockers #(map height-at %))
                          is-blocked (blocked? height potential-blocker-heights)]]
                {:coord                     [x y]
                 :height                    height
                 :p-blockers                potential-blockers
                 :potential-blocker-heights potential-blocker-heights
                 :visible                   (not is-blocked)})))))

(println (count (filter :visible (count-visible-trees forest))))