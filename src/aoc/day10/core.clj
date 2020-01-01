(ns aoc.day10.core
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))


(defn same-x [a b]
  (= (float (a 0)) (float (b 0))))


(defn same-y [a b]
  (= (float (a 1)) (float (b 1))))


(defn same-point [a b]
  (and (same-x a b) (same-y a b)))


(defn greater-x [a b]
  (> (b 0) (a 0)))


(defn greater-y [a b]
  (> (b 1) (a 1)))


(defn less-x [a b]
  (< (b 0) (a 0)))


(defn less-y [a b]
  (< (b 1) (a 1)))


(defn diff-x [a b]
  (- (b 0) (a 0)))


(defn diff-y [a b]
  (- (b 1) (a 1)))


(defn diff [a b]
  [(diff-x a b) (diff-y a b)])


(defn add-vector [current delta]
  [(+ (current 0) (delta 0)) (+ (current 1) (delta 1))])


(defn dot-product [a b]
  (+ (* (a 0) (b 0)) (* (a 1) (b 1))))


(defn square [x]
  (* x x))


(defn magnitude [a]
  (Math/sqrt (+ (square (a 0)) (square (a 1)))))


(defn cross-product [a b]
  (* (magnitude a) (magnitude b)))


(def pi 3.141592653589793)


(defn angle-between [a b]
  (let [dp (dot-product a b)
        angle (Math/acos (/ dp (cross-product a b)))]
    (if (less-x a b)
      (- (* 2 pi) angle)
      angle)))


(defn satellite-angle [a b]
  (angle-between [0 -1] (diff a b)))


(defn same-x-delta [a b]
  (if (greater-y a b)
    [0 1]
    [0 -1]))


(defn same-y-delta [a b]
  (if (greater-x a b)
    [1 0]
    [-1 0]))


(defn slope [a b]
  (/ (diff-y a b) (diff-x a b)))


(defn ensure-positive [value]
  (if (< value 0)
    (* -1 value)
    value))


(defn ensure-negative [value]
  (if (> value 0)
    (* -1 value)
    value))


(defn diff-delta-y [a b]
  (let [slp (slope a b)]
    (if (greater-y a b)
      (ensure-positive slp)
      (ensure-negative slp))))


(defn diff-delta [a b]
  (if (greater-x a b)
    [1 (diff-delta-y a b)]
    [-1 (diff-delta-y a b)]))


(defn delta-diff [a b]
  (if (same-x a b)
    (same-x-delta a b)
    (if (same-y a b)
      (same-y-delta a b)
      (diff-delta a b))))


(defn advance-point [a b current]
  (add-vector current (delta-diff a b)))


(defn satellite-to-keyword [[x y]]
  (keyword (str (str x) "-" (str y))))


(defn keyword-to-satellite [kw]
  (into [] (map #(Integer/parseInt %1) (str/split (name kw) #"-"))))


(defn path-between [a b]
  (loop [current a
         path []]
    (if (same-point current b)
      (conj path current)
      (recur (advance-point a b current) (conj path current)))))


(defn path-between-set [a b]
  (into #{} (path-between a b)))


(defn satellites-on-path [satellites a b]
  (count (set/intersection satellites (path-between-set a b))))


(defn is-visible [satellites source dest]
  (if (same-point source dest)
    false
    (= 2 (satellites-on-path satellites source dest))))


(defn visible-satellites-count [satellites source]
  (reduce (fn [c s] (if (is-visible satellites source s) (inc c) c)) 0 satellites))


(defn visible-satellites [satellites source]
  (reduce (fn [c s] (if (is-visible satellites source s) (conj c s) c)) #{} satellites))


(defn visible-satellites-angles [source visibles]
  (reduce (fn [c s] (assoc c (satellite-angle source s) (satellite-to-keyword s))) {} visibles))


(defn visible-satellites-vapor-order [angles]
  (reduce (fn [c a] (conj c (keyword-to-satellite (get angles a)))) [] (sort (keys angles))))


(defn vapored? [vapored candidate]
  (> (count (set/intersection vapored #{candidate}))) 0)


(defn remove-vapored-satellites [satellites vapored]
  (set/difference satellites vapored))


(defn visibles-to-vapor-order [visibles source]
  (visible-satellites-vapor-order (visible-satellites-angles source visibles)))


(defn vaporise [satellites source order]
  (if (= 1 (count satellites))
    order
    (let [visibles (visible-satellites satellites source)
          vapored (visibles-to-vapor-order visibles source)]
      (vaporise (remove-vapored-satellites satellites visibles) source (into [] (concat order vapored))))))


(defn visible-satellites-map [satellites]
  (reduce #(assoc %1 (satellite-to-keyword %2) (visible-satellites-count satellites %2)) {} satellites))


(defn satellites-on-line [line]
  (reduce (fn [a i] (if (= "#" (line i)) (conj a i) a)) [] (range (count line))))


(defn satellites-on-line-y [line y]
  (reduce (fn [a x] (conj a [x y])) [] (satellites-on-line line)))


(defn lines-to-satellites [lines]
  (reduce (fn [a i] (into a (satellites-on-line-y (lines i) i))) #{} (range (count lines))))


(defn lines-to-visible-satellites-map [lines]
  (visible-satellites-map (lines-to-satellites lines)))


(defn line-to-chars [line]
  (into [] (str/split line #"")))


(defn lines-to-lines [lines]
  (reduce (fn [a l] (conj a (line-to-chars l))) [] lines))


(defn string-to-lines [string]
  (str/split string #"\n"))


(defn file-to-string [filename]
  (str/trim (slurp filename)))


(defn file-to-lines [filename]
  (lines-to-lines (string-to-lines (file-to-string filename))))


(defn file-to-visible-satellites-map [filename]
  (lines-to-visible-satellites-map (file-to-lines filename)))


(defn swap-key-vals [board]
  (reduce-kv #(assoc %1 %3 %2) {} board))


(defn file-to-most [filename]
  (let [smap (file-to-visible-satellites-map filename)
        swapped (swap-key-vals smap)
        sorted (sort (keys swapped))]
    (last sorted)))


(defn file-to-best [filename]
  (let [smap (file-to-visible-satellites-map filename)
        swapped (swap-key-vals smap)
        sorted (sort (keys swapped))]
    (keyword-to-satellite (get swapped (last sorted)))))


(defn file-to-satellites [filename]
  (lines-to-satellites (file-to-lines filename)))


(defn file-to-vapor-order [filename source]
  (vaporise (file-to-satellites filename) source []))


(defn file-to-nth-vaporised [filename source n]
  (nth (file-to-vapor-order filename source) 199))


(defn file-to-nth-vaporised-sum [filename source n]
  (let [satellite (file-to-nth-vaporised filename source n)]
    (+ (* (satellite 0) 100) (satellite 1))))


(defn run []
  (println "Day 10, part 1: best is" (file-to-best "src/aoc/day10/input.txt") "with" (file-to-most "src/aoc/day10/input.txt"))
  (println "Day 10, part 2:" (file-to-nth-vaporised-sum "src/aoc/day10/input.txt" [26 29] 199)))
