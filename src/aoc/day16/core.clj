(ns aoc.day16.core
  (:require
    [clojure.string :as str]))


(def base-pattern
  [0 1 0 -1])


(defn position-to-pattern [position]
  (reduce
    (fn [c p]
      (into c (repeat position p)))
    []
    base-pattern))


(defn next-pattern-index [pattern current]
  (let [index (+ 1 current)
        last (- (count pattern) 1)]
    (if (> index last)
      0
      index)))


(defn string-to-ints [string]
  (into [] (map #(Integer/parseInt %1) (str/split string #""))))


(defn last-digit [inum]
  (last (string-to-ints (str (Math/abs inum)))))


(defn process-row [row position]
  (let [pattern (position-to-pattern position)
        last (count row)]
    (loop [row-index 0
           pattern-index (next-pattern-index pattern 0)
           out []]
      (if (= row-index last)
        out
        (let [multiplier (get pattern pattern-index)]
          (recur (inc row-index) (next-pattern-index pattern pattern-index) (conj out (* multiplier (get row row-index)))))))))


(defn row-to-last-digit [row position]
  (last-digit (reduce + (process-row row position))))


(defn row-to-row [row]
  (let [last (count row)]
    (loop [position 1
           out []]
      (if (> position last)
        out
        (recur (inc position) (conj out (row-to-last-digit row position)))))))


(defn row-phases [row phases]
  (if (= 0 phases)
    row
    (row-phases (row-to-row row) (dec phases))))


(defn string-phases [string phases]
  (row-phases (string-to-ints string) phases))


(defn first-eight [row]
  (take 8 row))


(defn ints-to-string [ints]
  (str/join (map str ints)))


(defn row-first-eight [row]
  (ints-to-string (first-eight row)))


(defn file-to-string [filename]
  (str/trim (slurp filename)))


(defn file-phases [filename phases]
  (string-phases (file-to-string filename) phases))


(defn file-phases-first-eight [filename phases]
  (row-first-eight (file-phases filename phases)))


(defn run []
  (println "Day 16, part 1:" (file-phases-first-eight "src/aoc/day16/input.txt" 100))
  (println "Day 16, part 2:"))