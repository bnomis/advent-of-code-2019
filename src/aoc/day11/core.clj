(ns aoc.day11.core
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [clojure.core.async :as async]))


(def state-init {:memory []
                 :pointer 0
                 :relative-base 0
                 :inputs []
                 :input-channel nil
                 :outputs []
                 :output-channel nil
                 :hull {}
                 :robot {:position [0 0]
                         :direction :up
                         :start-on-white false}})



(defn add-instructions [state instructions]
  (assoc-in state [:memory] instructions))


(defn add-input [state value]
  (-> state
    (assoc-in [:phase] nil)
    (update-in [:inputs] conj value)))


(defn add-input-channel [state channel]
  (assoc-in state [:input-channel] channel))


(defn set-start-on-white [state value]
  (assoc-in state [:robot :start-on-white] value))


(defn is-start-on-white [state]
  (get-in state [:robot :start-on-white]))


(defn position-to-keyword [[x y]]
  (keyword (str (str x) "," (str y))))


(defn position-x [[x y]]
  x)


(defn position-y [[x y]]
  y)


(defn keyword-to-position [kw]
  (into [] (map #(Integer/parseInt %1) (str/split (name kw) #","))))


(defn keyword-to-position-x [kw]
  (get (keyword-to-position kw) 0))


(defn keyword-to-position-y [kw]
  (get (keyword-to-position kw) 1))


(defn robot-position [state]
  (get-in state [:robot :position]))


(defn robot-direction [state]
  (get-in state [:robot :direction]))


(defn root-position [position]
  (and (= 0 (position 0)) (= 0 (position 1))))


(defn colour-at-position [state position]
  (let [kw (position-to-keyword position)
        colour (get-in state [:hull kw])]
    (if colour
      colour
      (if (and (root-position position) (is-start-on-white state))
        1
        0))))


(defn colour-at-robot [state]
  (colour-at-position state (robot-position state)))


(defn paint-at-position [state position colour]
  (assoc-in state [:hull (position-to-keyword position)] colour))


(defn paint-at-robot [state colour]
  (paint-at-position state (robot-position state) colour))


(defn number-of-paints [state]
  (count (:hull state)))


(defn add-position-delta [position delta]
  [(+ (position 0) (delta 0)) (+ (position 1) (delta 1))])


(defn update-robot-position [state delta]
  (update-in state [:robot :position] add-position-delta delta))


(defn move-robot-forward [state]
  (case (robot-direction state)
    :up (update-robot-position state [0 1])
    :left (update-robot-position state [-1 0])
    :right (update-robot-position state [1 0])
    :down (update-robot-position state [0 -1])))


(defn update-robot-direction [state direction]
  (assoc-in state [:robot :direction] direction))


(defn rotate-robot-left [state]
  (case (robot-direction state)
    :up (update-robot-direction state :left)
    :left (update-robot-direction state :down)
    :right (update-robot-direction state :up)
    :down (update-robot-direction state :right)))


(defn rotate-robot-right [state]
  (case (robot-direction state)
    :up (update-robot-direction state :right)
    :left (update-robot-direction state :up)
    :right (update-robot-direction state :down)
    :down (update-robot-direction state :left)))


(defn rotate-robot [state value]
  (case value
    0 (rotate-robot-left state)
    1 (rotate-robot-right state)))


(defn read-input [state]
  (colour-at-robot state))


(defn send-input [state value]
  (async/>!! (:input-channel state) value))


(defn send-output [state value]
  (async/>!! (:output-channel state) value))


(defn output-length [state]
  (count (:outputs state)))


(defn output-is-colour [state]
  (even? (output-length state)))


(defn add-output [state value]
  (update-in state [:outputs] conj value))


(defn handle-colour [state value]
  (-> state
    (paint-at-robot value)
    (add-output value)))


(defn handle-rotation [state value]
  (-> state
    (rotate-robot value)
    (move-robot-forward)
    (add-output value)))


(defn write-output [state value]
  (if (output-is-colour state)
    (handle-colour state value)
    (handle-rotation state value)))


(defn get-last-output [state]
  (last (get-in state [:outputs])))


(defn add-output-channel [state channel]
  (assoc-in state [:output-channel] channel))


(defn extend-memory [memory length]
  (if (= length 0)
    memory
    (extend-memory (conj memory 0) (dec length))))


(defn assure-memory [memory address]
  (let [length (count memory)]
    (if (> length address)
      memory
      (extend-memory memory (+ (- address length) 1)))))


(defn resize-memory [state address]
  (assoc-in state [:memory] (assure-memory (:memory state) address)))


(defn get-value-at [state address]
  (-> state
    (resize-memory address)
    (get-in [:memory address])))


(defn get-value-relative-base [state address]
  (get-value-at state (+ (:relative-base state) address)))


(defn store-value-at [state address value]
  (-> state
    (resize-memory address)
    (assoc-in [:memory address] value)))


(defn update-relative-base [state value]
  (update-in state [:relative-base] + value))


(defn advance-pointer [state advance]
  (update-in state [:pointer] + advance))


(defn set-pointer [state value]
  (assoc-in state [:pointer] value))


(defn get-value-at-pointer [state]
  (get-value-at state (:pointer state)))


(defn get-value-relative-to-pointer [state delta]
  (get-value-at state (+ (:pointer state) delta)))


(defn get-value-relative-to-pointer-with-mode [state delta mode]
  (let [value (get-value-relative-to-pointer state delta)]
    (case mode
      0 (get-value-at state value)
      1 value
      2 (get-value-relative-base state value))))


(defn get-value-relative-to-pointer-as-dest [state delta mode]
  (let [value (get-value-relative-to-pointer state delta)]
    (case mode
      0 value
      2 (+ value (:relative-base state)))))


(defn int-to-string [an-int]
  (str an-int))


(defn string-to-digits [a-string]
  (str/split a-string #""))


(defn string-list-to-list-of-ints [string-list]
  (map #(Integer/parseInt %1) string-list))


(defn string-to-list-of-ints [a-string]
  (string-list-to-list-of-ints (string-to-digits a-string)))


(defn int-to-list-of-ints [an-int]
  (into [] (string-to-list-of-ints (int-to-string an-int))))


(defn ints-to-int [ints]
  (Integer/parseInt (apply str (map int-to-string ints))))


(defn value-to-op [value]
  (let [ints (int-to-list-of-ints value)
        count (count ints)]
    (if (< count 3)
      value
      (ints-to-int (take-last 2 ints)))))


(defn value-to-mode [value arg]
  (let [ints (int-to-list-of-ints value)
        count (count ints)]
    (if (< count 3)
      0
      (let [remaining (- count 2)]
        (if (> arg remaining)
          0
          (nth (reverse (take remaining ints)) (- arg 1)))))))


(defn get-op [state]
  (value-to-op (get-value-at state (:pointer state))))


(defn add-op [state]
  (let [op (get-value-at-pointer state)
        mode1 (value-to-mode op 1)
        mode2 (value-to-mode op 2)
        mode3 (value-to-mode op 3)
        arg1 (get-value-relative-to-pointer-with-mode state 1 mode1)
        arg2 (get-value-relative-to-pointer-with-mode state 2 mode2)
        dest (get-value-relative-to-pointer-as-dest state 3 mode3)]
    (-> state
      (store-value-at dest (+ arg1 arg2))
      (advance-pointer 4))))


(defn multiply-op [state]
  (let [op (get-value-at-pointer state)
        mode1 (value-to-mode op 1)
        mode2 (value-to-mode op 2)
        mode3 (value-to-mode op 3)
        arg1 (get-value-relative-to-pointer-with-mode state 1 mode1)
        arg2 (get-value-relative-to-pointer-with-mode state 2 mode2)
        dest (get-value-relative-to-pointer-as-dest state 3 mode3)]
    (-> state
      (store-value-at dest (* arg1 arg2))
      (advance-pointer 4))))


(defn input-op [state]
  (let [op (get-value-at-pointer state)
        mode1 (value-to-mode op 1)
        value (read-input state)
        dest (get-value-relative-to-pointer-as-dest state 1 mode1)]
    (-> state
      (add-input value)
      (store-value-at dest value)
      (advance-pointer 2))))


(defn output-op [state]
  (let [op (get-value-at-pointer state)
        mode1 (value-to-mode op 1)
        value (get-value-relative-to-pointer-with-mode state 1 mode1)]
    (-> state
      (write-output value)
      (advance-pointer 2))))


(defn jump-if-true [state]
  (let [op (get-value-at-pointer state)
        mode1 (value-to-mode op 1)
        mode2 (value-to-mode op 2)
        arg1 (get-value-relative-to-pointer-with-mode state 1 mode1)
        arg2 (get-value-relative-to-pointer-with-mode state 2 mode2)]
    (if (= 0 arg1)
      (advance-pointer state 3)
      (set-pointer state arg2))))


(defn jump-if-false [state]
  (let [op (get-value-at-pointer state)
        mode1 (value-to-mode op 1)
        mode2 (value-to-mode op 2)
        arg1 (get-value-relative-to-pointer-with-mode state 1 mode1)
        arg2 (get-value-relative-to-pointer-with-mode state 2 mode2)]
    (if (= 0 arg1)
      (set-pointer state arg2)
      (advance-pointer state 3))))


(defn less-than [state]
  (let [op (get-value-at-pointer state)
        mode1 (value-to-mode op 1)
        mode2 (value-to-mode op 2)
        mode3 (value-to-mode op 3)
        arg1 (get-value-relative-to-pointer-with-mode state 1 mode1)
        arg2 (get-value-relative-to-pointer-with-mode state 2 mode2)
        dest (get-value-relative-to-pointer-as-dest state 3 mode3)
        state (if (< arg1 arg2)
                (store-value-at state dest 1)
                (store-value-at state dest 0))]
    (advance-pointer state 4)))


(defn equals-op [state dest arg1 arg2]
  (if (= arg1 arg2)
    (store-value-at state dest 1)
    (store-value-at state dest 0)))


(defn equals [state]
  (let [op (get-value-at-pointer state)
        mode1 (value-to-mode op 1)
        mode2 (value-to-mode op 2)
        mode3 (value-to-mode op 3)
        arg1 (get-value-relative-to-pointer-with-mode state 1 mode1)
        arg2 (get-value-relative-to-pointer-with-mode state 2 mode2)
        dest (get-value-relative-to-pointer-as-dest state 3 mode3)]
    (-> state
      (equals-op dest arg1 arg2)
      (advance-pointer 4))))


(defn add-to-relative-base [state value]
  (update-in state [:relative-base] + value))


(defn add-to-relative-base-value-at [state address]
  (-> state
    (add-to-relative-base (get-value-at state address))
    (advance-pointer 2)))


(defn add-to-relative-base-value [state value]
  (-> state
      (add-to-relative-base  value)
      (advance-pointer 2)))


(defn add-to-relative-base-value-relative-to-self [state value]
  (-> state
      (add-to-relative-base (get-value-at state (+ (:relative-base state) value)))
      (advance-pointer 2)))


(defn relative-base [state]
  (let [op (get-value-at-pointer state)
        mode1 (value-to-mode op 1)
        arg1 (get-value-relative-to-pointer state 1)]
    (case mode1
      0 (add-to-relative-base-value-at state arg1)
      1 (add-to-relative-base-value state arg1)
      2 (add-to-relative-base-value-relative-to-self state arg1))))


(defn exec-op [state op]
  (case op
    1 (add-op state)
    2 (multiply-op state)
    3 (input-op state)
    4 (output-op state)
    5 (jump-if-true state)
    6 (jump-if-false state)
    7 (less-than state)
    8 (equals state)
    9 (relative-base state)))


(defn print-state [state]
  (println "pointer:" (:pointer state) "op:" (get-value-at state (:pointer state)) "relative-base:" (:relative-base state) "inputs:" (:inputs state) "outputs:" (:outputs state)))


(defn run-amp [state]
  (let [phase (get-in state [:phase])]
    (loop [state state]
      ;(print-state state)
      (let [op (get-op state)]
        (if (= 99 op)
          state
          (recur (exec-op state op)))))))


(defn run-amp-in-thread [state]
  (async/thread (run-amp state)))


(defn run-amp-with-input [state value]
  (send-input state value)
  (run-amp state))


(defn make-amp [instructions input-channel output-channel]
  (-> state-init
    (add-instructions instructions)
    (add-input-channel input-channel)
    (add-output-channel output-channel)))


(defn make-channel []
  (async/chan 100))


(defn strings-to-instructions [strings]
  (into [] (map #(Long/parseLong %1) strings)))


(defn string-to-strings [string]
  (str/split string #","))


(defn string-to-instructions [string]
  (strings-to-instructions (string-to-strings string)))


(defn file-to-string [filename]
  (str/trim (slurp filename)))


(defn file-to-instructions [filename]
  (-> filename
      (file-to-string)
      (string-to-instructions)))


(defn run-string [string]
  (run-amp (make-amp (string-to-instructions string) (make-channel) (make-channel))))


(defn run-file [filename]
  (run-string (file-to-string filename)))


(defn run-string-start-white [string]
  (run-amp (set-start-on-white (make-amp (string-to-instructions string) (make-channel) (make-channel)) true)))


(defn run-file-start-white [filename]
  (run-string-start-white (file-to-string filename)))


(defn run-file-count-paints [filename]
  (let [state (run-file filename)]
    (number-of-paints state)))


(defn hull-x-values [state]
  (map keyword-to-position-x (keys (:hull state))))


(defn hull-y-values [state]
  (map keyword-to-position-y (keys (:hull state))))


(defn hull-x-min [state]
  (first (sort (hull-x-values state))))


(defn hull-x-max [state]
  (last (sort (hull-x-values state))))


(defn hull-y-min [state]
  (first (sort (hull-y-values state))))


(defn hull-y-max [state]
  (last (sort (hull-y-values state))))


(defn x-to-column [x x-min]
  (if (< x-min 0)
    (- x x-min)
    x))


(defn y-to-row [y y-max]
  (if (< y 0)
    (+ (Math/abs y) (Math/abs y-max))
    (- y y-max)))


(defn position-to-row-column [[x y] x-min y-max]
  [(y-to-row y y-max) (x-to-column x x-min)])


(defn set-hull-value [hull rc value]
  (assoc-in hull rc value))


(defn make-row [width]
  (into [] (repeat width 0)))


(defn make-hull [width height]
  (loop [count 0
         rows []]
    (if (= count height)
      rows
      (recur (inc count) (conj rows (make-row width))))))


(defn paint-hull [state]
  (let [x-min (hull-x-min state)
        x-max (hull-x-max state)
        y-min (hull-y-min state)
        y-max (hull-y-max state)
        width (+ (- x-max x-min) 1)
        height (+ (- y-max y-min) 1)
        hull (make-hull width height)]
    (reduce (fn [c kw] (set-hull-value c (position-to-row-column (keyword-to-position kw) x-min y-max) (get-in state [:hull kw]))) hull (keys (:hull state)))))


(defn print-hull [state]
  (let [hull (paint-hull state)
        lines (count hull)]
    (loop [count 0]
      (if (< count lines)
        (do
          (println (get hull count))
          (recur (inc count)))))))


(defn run-file-print-hull [filename]
  (let [state (run-file-start-white filename)]
    (print-hull state)))


(defn run []
  (println "Day 11, part 1:" (run-file-count-paints "src/aoc/day11/input.txt"))
  (println "Day 11, part 2:")
  (run-file-print-hull "src/aoc/day11/input.txt"))
