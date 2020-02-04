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


(defn str-to-int [string]
  (Integer/parseInt string))


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

; assume the offset is over half way
; we can ignore everything before it
; it just becomes a sum
(defn row-to-row-offset [row offset]
  (let [length (count row)]
    (loop [index 0
           out []
           sum (reduce + (drop offset row))]
      (if (= index length)
        out
        (if (< index offset)
          (recur (inc index) (conj out 0) sum)
          (recur (inc index) (conj out (mod sum 10)) (- sum (get row index))))))))


(defn row-phases [row phases]
  (if (= 0 phases)
    row
    (row-phases (row-to-row row) (dec phases))))


(defn row-phases-with-offset [row phases offset]
  (if (= 0 phases)
    row
    (row-phases-with-offset (row-to-row-offset row offset) (dec phases) offset)))


(defn string-phases [string phases]
  (row-phases (string-to-ints string) phases))


(defn first-eight [row]
  (take 8 row))


(defn first-x [row x]
  (take x row))


(defn ints-to-string [ints]
  (str/join (map str ints)))


(defn row-first-eight [row]
  (ints-to-string (first-eight row)))


(defn row-first-x-to-string [row x]
  (ints-to-string (first-x row x)))


(defn file-to-string [filename]
  (str/trim (slurp filename)))


(defn file-phases [filename phases]
  (string-phases (file-to-string filename) phases))


(defn file-phases-first-eight [filename phases]
  (row-first-eight (file-phases filename phases)))


(defn repeat-vector [vector repeats]
  (reduce (fn [c v] (into c vector)) [] (range repeats)))


(defn row-x-at-offset [row x offset]
  (ints-to-string (reduce (fn [c i] (conj c (get row (+ offset i)))) [] (range x))))


(defn string-repeat-offset [string phases repeats offset]
  (let [ints (string-to-ints string)
        vector (repeat-vector ints repeats)
        s (row-first-x-to-string ints offset)
        i (str-to-int s)
        row (row-phases-with-offset vector phases i)]
    (row-x-at-offset row 8 i)))


(defn file-repeat-offset [filename phases repeats offset]
  (string-repeat-offset (file-to-string filename) phases repeats offset))


(defn run []
  (println "Day 16, part 1:" (file-phases-first-eight "src/aoc/day16/input.txt" 100))
  (println "Day 16, part 2:" (file-repeat-offset "src/aoc/day16/input.txt" 100 10000 7)))