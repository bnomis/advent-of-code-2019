(ns aoc.day12.core
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [clojure.set :as set]))


(def moon-init {:position [0 0 0]
                :velocity [0 0 0]})


(defn vector-to-string [vec]
  (str/join " " (map str vec)))


(defn moon-to-string [moon]
  (str/join " " [(vector-to-string (:position moon)) (vector-to-string (:velocity moon))]))


(defn moons-to-string [moons]
  (str/join " " (map moon-to-string moons)))


(defn is-previous-state? [previous state]
  (let [last (count previous)]
    (loop [index 0]
      (if (= last index)
        false
        (if (= (get previous index) state)
          true
          (recur (inc index)))))))


(defn abs-sum [vec]
  (reduce + (map #(Math/abs %1) vec)))


(defn moon-potential [moon]
  (abs-sum (:position moon)))


(defn moon-kinetic [moon]
  (abs-sum (:velocity moon)))


(defn moon-energy [moon]
  (* (moon-potential moon) (moon-kinetic moon)))


(defn update-vector [source delta]
  [(+ (get source 0) (get delta 0))
   (+ (get source 1) (get delta 1))
   (+ (get source 2) (get delta 2))])


(defn axis-is-less [source candidate axis]
  (< (get candidate axis) (get source axis)))


(defn axis-is-greater [source candidate axis]
  (> (get candidate axis) (get source axis)))


(defn axis-is-equal [source candidate axis]
  (= (get candidate axis) (get source axis)))


(defn inc-axis [source axis]
  (update-in source [axis] inc))


(defn dec-axis [source axis]
  (update-in source [axis] dec))


(defn update-velocity-on-axis [pos1 pos2 velocity axis]
  (if (axis-is-greater pos1 pos2 axis)
    (inc-axis velocity axis)
    (if (axis-is-less pos1 pos2 axis)
      (dec-axis velocity axis)
      velocity)))


(defn update-velocity [pos1 pos2 velocity axis]
  (if (= axis 3)
    velocity
    (update-velocity pos1 pos2
                     (update-velocity-on-axis pos1 pos2 velocity axis)
                     (inc axis))))


(defn update-moon-position [moon]
  (assoc-in moon [:position] (update-vector (:position moon) (:velocity moon))))


(defn moons-vector [moons]
  (into [] (range (count moons))))


(defn velocity-pair [new-moons moons [m1 m2]]
  (assoc-in new-moons [m1 :velocity]
             (update-velocity
               (get-in moons [m1 :position])
               (get-in moons [m2 :position])
               (get-in new-moons [m1 :velocity])
               0)))


(defn velocity-pair-flip [new-moons moons [m1 m2]]
  (-> new-moons
    (velocity-pair moons [m1 m2])
    (velocity-pair moons [m2 m1])))


(defn velocity-step [moons]
  (reduce
    #(velocity-pair-flip %1 moons %2)
    moons
    (combo/combinations (moons-vector moons) 2)))


(defn position-step [moons]
  (into [] (map update-moon-position moons)))


(defn moons-step [moons]
  (-> moons
    (velocity-step)
    (position-step)))


(defn step-moons [moons steps]
  (if (= 0 steps)
    moons
    (step-moons (moons-step moons) (dec steps))))


(defn remove-angles [line]
  (str/replace line #"[<>]" ""))

(defn line-to-coords [line]
  (str/split line #","))


(defn axis-to-vector-position [axis]
  (case axis
    "x" 0
    "y" 1
    "z" 2))


(defn coord-to-axis-and-value [coord]
  (let [parts (str/split coord #"=")]
    [(axis-to-vector-position (str/trim (get parts 0)))
     (Integer/parseInt (str/trim (get parts 1)))]))


(defn update-position-with-vector [pos vec]
  (assoc pos (get vec 0) (get vec 1)))


(defn coords-to-position [coords]
  (let [pos-vec (map coord-to-axis-and-value coords)]
    (reduce update-position-with-vector [0 0 0] pos-vec)))


(defn create-moon [position]
  (assoc-in moon-init [:position]  position))


(defn line-to-moon [line]
  (-> line
    (remove-angles)
    (line-to-coords)
    (coords-to-position)
    (create-moon)))


(defn lines-to-moons [lines]
  (into [] (map line-to-moon lines)))


(defn string-to-lines [string]
  (str/split string #"\n"))


(defn string-to-moons [string]
  (lines-to-moons (string-to-lines string)))


(defn file-to-string [filename]
  (str/trim (slurp filename)))


(defn file-to-moons [filename]
  (string-to-moons (file-to-string filename)))


(defn file-to-moons-steps [filename steps]
  (step-moons (file-to-moons filename) steps))


(defn file-to-energy-steps [filename steps]
  (reduce + (map moon-energy (file-to-moons-steps filename steps))))


(defn found-cycles [cycles]
  (if (and (get-in cycles [0]) (get-in cycles [1]) (get-in cycles [2]))
    true
    false))


(defn moon-to-axis [moon axis]
  [(get-in moon [:position axis]) (get-in moon [:velocity axis])])


(defn moons-to-axis [moons axis]
  (into [] (map #(moon-to-axis %1 axis) moons)))


(defn same-axis [m1 m2 axis]
  (= (moons-to-axis m1 axis) (moons-to-axis m2 axis)))


(defn check-cycle-axis [initial current loop-count cycles axis]
  (if (= 0 loop-count)
    cycles
    (if (same-axis initial current axis)
      (assoc-in cycles [axis] loop-count)
      cycles)))


(defn check-cycle [previous current loop-count cycles axis]
  (if (get-in cycles [axis])
    cycles
    (check-cycle-axis previous current loop-count cycles axis)))


(defn check-cycles [cycles initial current loop-count]
  (reduce #(check-cycle initial current loop-count %1 %2) cycles [0 1 2]))


; https://rosettacode.org/wiki/Least_common_multiple
(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& v]
  (reduce lcm v))


; https://en.wikipedia.org/wiki/Least_common_multiple
(defn lcm-set [a b c]
  (loop [multiplier 2
         a-set #{a}
         b-set #{b}
         c-set #{c}]
    (let [inter (set/intersection a-set b-set c-set)]
      (if (> (count inter) 0)
        (first (sort inter))
        (recur (inc multiplier)
               (conj a-set (* a multiplier))
               (conj b-set (* b multiplier))
               (conj c-set (* c multiplier)))))))


(defn cycles-to-lcm [cycles]
  (lcmv (get cycles 0) (get cycles 1) (get cycles 2)))


; only need to check the current step against the initial
(defn file-to-cycles [filename]
  (let [initial (file-to-moons filename)]
    (loop [cycles {}
           current initial
           loop-count 0]
      (if (found-cycles cycles)
        cycles
        (recur (check-cycles cycles initial current loop-count) (moons-step current) (inc loop-count))))))


(defn file-to-repeat [filename]
  (cycles-to-lcm (file-to-cycles filename)))


(defn run []
  (println "Day 12, part 1:" (file-to-energy-steps "src/aoc/day12/input.txt" 1000))
  (println "Day 12, part 2:" (file-to-repeat "src/aoc/day12/input.txt")))