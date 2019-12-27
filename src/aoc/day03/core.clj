(ns aoc.day03.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))


(def vectors {:R [1 0]
              :L [-1 0]
              :U [0 1]
              :D [0 -1]})


(def state-init {:steps []
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


(defn update-steps [state]
  (let [kw (position-to-keyword (:current state))]
    (update state :steps conj kw)))


(defn walk-path [state path]
  (let [vector (path-to-vector path)]
    (loop [state state
           count (path-to-length path)]
      (if (= 0 count)
        state
        (recur (-> state
                 (update-current vector)
                 (update-steps)) (dec count))))))


(defn follow-paths [paths]
  (:steps (reduce walk-path state-init paths)))


(defn run-lines [lines]
  (map follow-paths lines))


(defn list-to-set [some-list]
  (into #{} some-list))


(defn lists-to-sets [list-of-lists]
  (map list-to-set list-of-lists))


(defn intersects [m k v]
  (if (< v 2)
    m
    (assoc m k v)))


(defn board-intersections [boards]
  (apply set/intersection boards))


(defn distance [m k v]
  (assoc m k (keyword-to-distance k)))


(defn board-distances [board]
  (map keyword-to-distance board))


(defn swap-key-vals [board]
  (reduce-kv #(assoc %1 %3 %2) {} board))


(defn sort-board [board]
  (sort board))


(defn closest-point [sorted-board]
  (first sorted-board))


(defn walk-the-lines [lines]
  (-> lines
      (lines-to-paths)
      (run-lines)))


(defn intersect-the-walks [walks]
  (-> walks
      (lists-to-sets)
      (board-intersections)))


(defn steps-to-intersection [walk intersection]
  (+ 1 (.indexOf walk intersection)))


(defn walk-steps-to-intersections [walks intersection]
  (reduce + (map #(steps-to-intersection %1 intersection) walks)))

(defn lines-to-closest [lines]
  (-> lines
      (lines-to-paths)
      (run-lines)
      (lists-to-sets)
      (board-intersections)
      (board-distances)
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


(defn lines-to-shortest [lines]
  (let [walks (walk-the-lines lines)
        intersects (intersect-the-walks walks)
        steps (map #(walk-steps-to-intersections walks %1) intersects)]
    (first (sort steps))))


(defn string-to-shortest [string]
  (-> string
      (string-to-lines)
      (lines-to-shortest)))


(defn file-to-shortest [filename]
  (-> filename
      (file-to-string)
      (string-to-shortest)))


(defn run []
  (println "Day 03, part 1:" (file-to-closest "src/aoc/day03/input.txt"))
  (println "Day 03, part 2:" (file-to-shortest "src/aoc/day03/input.txt")))
