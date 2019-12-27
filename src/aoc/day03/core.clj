(ns aoc.day03.core
  (:require [clojure.string :as str
             clojure.set :as set]))


(def vectors {:R [1 0]
              :L [-1 0]
              :U [0 1]
              :D [0 -1]})


(def state-init {:board {}
                 :current [0 0]})


(defn file-to-string [filename]
  (str/trim (slurp filename)))


(defn string-to-lines [string]
  (str/split string #"\n"))


(defn line-to-paths [line]
  (str/split line #","))


(defn lines-to-paths [lines]
  (map line-to-paths lines))


(defn path-to-direction [path]
  (str (first path)))


(defn direction-to-keyword [direction]
  (keyword direction))


(defn keyword-to-vector [keyword]
  (keyword vectors))


(defn path-to-vector [path]
  (keyword-to-vector (direction-to-keyword (path-to-direction path))))


(defn path-to-length [path]
  (Integer/parseInt (str/join (rest path))))


(defn position-to-keyword [position]
  (keyword (str (str (first position)) "+" (str (second position)))))


(defn keyword-to-position [kw]
  (map #(Integer/parseInt %) (str/split (name kw) #"\+")))


(defn abs-add [a b]
  (+ (Math/abs a) (Math/abs b)))


(defn keyword-to-distance [kw]
  (reduce abs-add (keyword-to-position kw)))


(defn update-current [state delta]
  (let [current (:current state)]
    (assoc state :current (update (update current 0 + (first delta)) 1 + (second delta)))))


(defn update-board [state]
  (let [kw (position-to-keyword (:current state))
        board (:board state)]
    (if (kw board)
      (assoc state :board (update board kw inc))
      (assoc state :board (assoc board kw 1)))))


(defn walk-path [state path]
  (let [vector (path-to-vector path)]
    (loop [state state
           count (path-to-length path)]
      (if (= 0 count)
        state
        (recur (-> state
                 (update-current vector)
                 (update-board)) (dec count))))))


(defn follow-paths [state paths]
  (reduce walk-path (assoc state :current [0 0]) paths))


(defn run-lines [lines]
  (let [state (reduce follow-paths state-init lines)]
    (:board state)))


(defn intersects [m k v]
  (if (< v 2)
    m
    (assoc m k v)))


(defn board-intersections [board]
  (reduce-kv intersects {} board))


(defn distance [m k v]
  (assoc m k (keyword-to-distance k)))


(defn board-distances [board]
  (reduce-kv distance {} board))


(defn swap-key-vals [board]
  (reduce-kv #(assoc %1 %3 %2) {} board))


(defn sort-board [board]
  (into (sorted-map) board))


(defn closest-point [sorted-board]
  (first (first sorted-board)))


(defn lines-to-closest [lines]
  (-> lines
      (lines-to-paths)
      (run-lines)
      (board-intersections)
      (board-distances)
      (swap-key-vals)
      (sort-board)
      (closest-point)))


(defn string-to-closest [string]
  (-> string
    (string-to-lines)
    (lines-to-closest)))


(defn file-to-closest [filename]
  (-> filename
      (file-to-string)
      (string-to-closest)))


(defn run []
  (println "Day 03, part 1:" (file-to-closest "src/aoc/day03/input.txt")))
