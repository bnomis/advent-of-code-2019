(ns aoc.day02.core
  (:require [clojure.string :as str]))


(defn doop [val op arg1 arg2]
  (if (= op 1)
    (+ arg1 arg2)
    (* arg1 arg2)))


(defn run-program [input]
  (loop [out input
         index 0
         op (get input index)]
    (if (= 99 op)
      out
      (let [arg1 (get out (get out (+ index 1)))
            arg2 (get out (get out (+ index 2)))
            dest (get out (+ index 3))
            out (update out dest
                 doop
                 op
                 arg1
                 arg2)]
        (recur
          out
          (+ index 4)
          (get out (+ index 4)))))))


(defn preprocess [input noun verb]
  (assoc (assoc input 1 noun) 2 verb))


(defn get-result [input noun verb]
  (get (run-program (preprocess input noun verb)) 0))


(defn hunt-verb [initial target noun]
  (loop [verb 0
         result (get-result initial noun verb)]
    (if (= result target)
      verb
      (if (< verb 99)
        (recur (inc verb) (get-result initial noun (+ 1 verb)))))))


(defn hunt [initial target]
  (loop [noun 0
         verb (hunt-verb initial target noun)]
    (if verb
      [noun verb]
      (if (< noun 99)
        (recur (inc noun) (hunt-verb initial target (+ 1 noun)))))))


(defn run []
  (let [input (str/trim (slurp "src/aoc/day02/input.txt"))
        numbers (str/split input #",")
        program (vec (map #(Integer/parseInt %) numbers))
        [noun verb] (hunt program 19690720)]
    (println "Day 02, part 1:" (get-result program 12 2))
    (println "Day 02, part 2:" (+ verb (* noun 100)))))
