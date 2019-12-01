(ns aoc.day01.core
  (:require [clojure.string :as str]))


(defn mass-to-fuel [mass]
  (- (quot mass 3) 2))


(defn mass-to-fuel2 [mass]
  (loop [fuel (mass-to-fuel mass)
         out []]
    (if (< fuel 0)
      (reduce + out)
      (recur (mass-to-fuel fuel) (conj out fuel)))))


(defn run []
  (let [input (slurp "src/aoc/day01/input.txt")
        numbers (str/split input #"\n")
        ints (map #(Integer/parseInt %) numbers)
        fuel (reduce + (map mass-to-fuel ints))
        fuel2 (reduce + (map mass-to-fuel2 ints))]
    (println "Day 01, part 1:" fuel)
    (println "Day 01, part 2:" fuel2)))
