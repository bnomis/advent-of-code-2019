(ns aoc.day04.core
  (:require [clojure.string :as str]))


(defn int-to-string [an-int]
  (str an-int))


(defn string-to-int [a-string]
  (Integer/parseInt a-string))


(defn string-to-digits [a-string]
  (str/split a-string #""))


(defn string-list-to-list-of-ints [string-list]
  (map #(Integer/parseInt %1) string-list))


(defn string-to-list-of-ints [a-string]
  (string-list-to-list-of-ints (string-to-digits a-string)))


(defn int-to-list-of-ints [an-int]
  (into [] (string-to-list-of-ints (int-to-string an-int))))


(defn equal-digits [numbers index-a index-b]
  (= (get numbers index-a) (get numbers index-b)))


(defn adjacent? [numbers]
  (let [count (count numbers)
        last (- count 1)]
    (loop [index 0]
      (if (= index last)
        false
        (if (equal-digits numbers index (+ index 1))
          true
          (recur (inc index)))))))


(defn decrease-digits [numbers index-a index-b]
  (< (get numbers index-b) (get numbers index-a)))


(defn no-decrease? [numbers]
  (let [count (count numbers)
        last (- count 1)]
    (loop [index 0]
      (if (= index last)
        true
        (if (decrease-digits numbers index (+ index 1))
          false
          (recur (inc index)))))))


(defn runner [state number]
  (if (= number (:current state))
    (update state :run conj number)
    (-> state
      (update :runs conj (:run state))
      (assoc :run [number])
      (assoc :current number))))


(defn numbers-to-runs [numbers]
  (let [start (get numbers 0)
        state (reduce runner {:current start :runs [] :run [start]} (rest numbers))]
    (conj (:runs state) (:run state))))


(defn length-is-two [run]
  (= 2 (count run)))


(defn run-of-length-two? [numbers]
  (if (> (count (filter length-is-two (numbers-to-runs numbers))) 0)
    true
    false))


(defn qualifies? [number]
  (let [numbers (int-to-list-of-ints number)]
    (and (adjacent? numbers) (no-decrease? numbers))))


(defn find-passwords [from to]
  (filter qualifies? (range from (+ 1 to))))


(defn count-passwords [from to]
  (count (find-passwords from to)))


(defn qualifies-part-2? [number]
  (let [numbers (int-to-list-of-ints number)]
    (and (run-of-length-two? numbers) (no-decrease? numbers))))


(defn find-passwords-part-2 [from to]
  (filter qualifies-part-2? (range from (+ 1 to))))


(defn count-passwords-part-2 [from to]
  (count (find-passwords-part-2 from to)))


(defn run []
  (println "Day 04, part 1:" (count-passwords 136818 685979))
  (println "Day 04, part 2:" (count-passwords-part-2 136818 685979)))
