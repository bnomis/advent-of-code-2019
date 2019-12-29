(ns aoc.day06.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))


(defn add-next-to-node [node next]
  (assoc node :next next))


(defn create-node-with-next [next]
  {:next next
   :prev nil})


(defn add-next [state from to]
  (if (from state)
    (update-in state [from] add-next-to-node to)
    (assoc-in state [from] (create-node-with-next to))))


(defn add-prev-to-node [node prev]
  (assoc node :prev prev))


(defn create-node-with-prev [prev]
  {:next nil
   :prev prev})


(defn add-prev [state from to]
  (if (from state)
    (update-in state [from] add-prev-to-node to)
    (assoc-in state [from] (create-node-with-prev to))))


(defn add-pair [state [from to]]
  (-> state
    (add-next from to)
    (add-prev to from)))


(defn get-prev [state from]
  (get-in state [from :prev]))


(defn count-steps-to-com [state from total]
  (let [prev (get-prev state from)]
    (if-not prev
      total
      (count-steps-to-com state prev (inc total)))))


(defn steps-to-com [state from steps]
  (let [prev (get-prev state from)]
    (if-not prev
      steps
      (steps-to-com state prev (conj steps prev)))))


(defn count-steps-to-dest [state dest from total]
  (let [prev (get-prev state from)]
    (if (= dest prev)
      (inc total)
      (count-steps-to-dest state dest prev (inc total)))))


(defn common-steps [state you santa]
  (let [your-steps-to-com (steps-to-com state you [])
        santas-steps-to-com (steps-to-com state santa [])
        your-set (into #{} your-steps-to-com)
        santas-set (into #{} santas-steps-to-com)]
    (set/intersection your-set santas-set)))


(defn total-step-count [state dest you santa]
  (+ (count-steps-to-dest state dest you 0) (count-steps-to-dest state dest santa 0)))


(defn steps-between [state you santa]
  (map #(total-step-count state %1 you santa) (common-steps state you santa)))


(defn least-steps-between [state you santa]
  (- (first (sort (steps-between state you santa))) 2))


(defn state-to-steps [state]
  (reduce + (map #(count-steps-to-com state %1 0) (keys state))))


(defn pairs-to-state [pairs]
  (reduce add-pair {} pairs))


(defn split-pair [string]
  (map keyword (str/split string #"\)")))


(defn string-to-pair [string]
  (into [] (split-pair string)))


(defn split-lines [string]
  (str/split string #"\n"))


(defn multiline-string-to-pairs [string]
  (into [] (map string-to-pair (split-lines string))))


(defn multiline-string-to-steps [string]
  (state-to-steps (pairs-to-state (multiline-string-to-pairs string))))


(defn file-to-string [filename]
  (str/trim (slurp filename)))


(defn file-to-steps [filename]
  (multiline-string-to-steps (file-to-string filename)))


(defn file-to-state [filename]
  (pairs-to-state (multiline-string-to-pairs (file-to-string filename))))


(defn file-to-least-steps [filename you santa]
  (let [state (file-to-state filename)]
    (least-steps-between state you santa)))


(defn run []
  (println "Day 06, part 1:" (file-to-steps "src/aoc/day06/input.txt")))