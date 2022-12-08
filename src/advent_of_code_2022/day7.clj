(ns advent-of-code-2022.day7
  (:require [clojure.java.io :as io]))

(def state
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
                                        dir-name (get matcher 1)]
                                    (assoc state :current-directory (conj (:current-directory state) dir-name)))
    (re-matches #"dir .*" line) state
    (re-matches #"\d+ .*" line) (let [matcher (re-matches #"(\d+) .*" line)
                                      size (Integer/parseInt (get matcher 1))
                                      dir (state :current-directory)]
                                  (add-file state dir size))
    (re-matches #"\$ ls" line) state))

(println (handle {:current-directory '("foo" "bar")} "$ cd .."))
(println (handle {:current-directory '("foo" "bar")} "$ cd baz"))
(println (handle {:current-directory '("foo" "bar")} "1234 foo.txt"))

(defn read-input [fname f]
  (with-open [rdr (clojure.java.io/reader (io/resource fname))]
    (f (line-seq rdr))))

(def results (read-input "day7.txt" #(reduce handle state %)))

(println (->> (dissoc results :current-directory)
              (filter #(let [[dir size] %] (< size 100000)))
              (map #(get % 1))
              (reduce +)))