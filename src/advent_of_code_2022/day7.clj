(ns advent-of-code-2022.day7
  (:require [clojure.java.io :as io]))

(def initial-state
  {:current-directory nil})

(defn add-file [state dir size]
  (if (empty? dir)
    state
    (let [prev-size (get state dir 0)
          added (assoc state dir (+ size prev-size))]
      (add-file added (drop 1 dir) size))))

(defn handle [state line]
  (cond
    (re-matches #"\$ cd \.\." line) (assoc state :current-directory (drop 1 (:current-directory state)))
    (re-matches #"\$ cd .*" line) (let [matcher (re-matches #"\$ cd (.*)" line)
                                        dir-name (second matcher)]
                                    (assoc state :current-directory (conj (:current-directory state) dir-name)))
    (re-matches #"dir .*" line) state
    (re-matches #"\d+ .*" line) (let [matcher (re-matches #"(\d+) .*" line)
                                      size (Integer/parseInt (second matcher))
                                      dir (state :current-directory)]
                                  (add-file state dir size))
    (re-matches #"\$ ls" line) state))

(defn read-input [fname f]
  (with-open [rdr (clojure.java.io/reader (io/resource fname))]
    (f (line-seq rdr))))

(def results (dissoc (read-input "day7.txt" #(reduce handle initial-state %)) :current-directory))

(println (->> results
              (map second)
              (filter #(< % 100000))
              (reduce +)))

(def need-to-free
  (let [disk-size 70000000
        need-for-update 30000000
        used (results '("/"))
        currently-free (- disk-size used)]
    (- need-for-update currently-free)))

(println (->> results
              (map second)
              (filter #(> % need-to-free))
              (reduce min)))