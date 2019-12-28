(ns aoc.day05.core
  (:require [clojure.string :as str]))


(defn file-to-string [filename]
  (str/trim (slurp filename)))


(defn string-to-strings [string]
  (str/split string #","))


(defn strings-to-instructions [strings]
  (into [] (map #(Integer/parseInt %1) strings)))


(defn file-to-instructions [filename]
  (-> filename
    (file-to-string)
    (string-to-strings)
    (strings-to-instructions)))


(defn get-value-at [state address]
  (get-in state [:memory address]))


(defn store-value-at [state address value]
  (assoc-in state [:memory address] value))


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
      1 value)))


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
        arg1 (get-value-relative-to-pointer-with-mode state 1 mode1)
        arg2 (get-value-relative-to-pointer-with-mode state 2 mode2)
        dest (get-value-relative-to-pointer state 3)]
    (-> state
      (store-value-at dest (+ arg1 arg2))
      (advance-pointer 4))))


(defn multiply-op [state]
  (let [op (get-value-at-pointer state)
        mode1 (value-to-mode op 1)
        mode2 (value-to-mode op 2)
        arg1 (get-value-relative-to-pointer-with-mode state 1 mode1)
        arg2 (get-value-relative-to-pointer-with-mode state 2 mode2)
        dest (get-value-relative-to-pointer state 3)]
    (-> state
        (store-value-at dest (* arg1 arg2))
        (advance-pointer 4))))


(defn read-input []
  (print "Input:> ")
  (flush)
  (Integer/parseInt (read-line)))


(defn input-to-state [state]
  (if (:input state)
    state
    (assoc-in state [:input] (read-input))))


(defn input-op [state]
  (let [state (input-to-state state)
        dest (get-value-relative-to-pointer state 1)]
    (-> state
      (store-value-at dest (:input state))
      (advance-pointer 2))))


(defn output-op [state]
  (let [op (get-value-at-pointer state)
        mode1 (value-to-mode op 1)
        arg1 (get-value-relative-to-pointer-with-mode state 1 mode1)]
    (println arg1)
    (advance-pointer state 2)))


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
        arg1 (get-value-relative-to-pointer-with-mode state 1 mode1)
        arg2 (get-value-relative-to-pointer-with-mode state 2 mode2)
        dest (get-value-relative-to-pointer state 3)
        state (if (< arg1 arg2)
                (store-value-at state dest 1)
                (store-value-at state dest 0))]
    (advance-pointer state 4)))


(defn equals [state]
  (let [op (get-value-at-pointer state)
        mode1 (value-to-mode op 1)
        mode2 (value-to-mode op 2)
        arg1 (get-value-relative-to-pointer-with-mode state 1 mode1)
        arg2 (get-value-relative-to-pointer-with-mode state 2 mode2)
        dest (get-value-relative-to-pointer state 3)
        state (if (= arg1 arg2)
                (store-value-at state dest 1)
                (store-value-at state dest 0))]
    (advance-pointer state 4)))


(defn exec-op [state op]
  (case op
    1 (add-op state)
    2 (multiply-op state)
    3 (input-op state)
    4 (output-op state)
    5 (jump-if-true state)
    6 (jump-if-false state)
    7 (less-than state)
    8 (equals state)))


(defn run-state [state]
  (loop [state state]
    (let [op (get-op state)]
      (if (= 99 op)
        (:memory state)
        (recur (exec-op state op))))))


(defn run-program
  ([instructions]
   (run-state {:memory instructions
               :pointer 0}))
  ([instructions input]
   (run-state {:memory instructions
               :pointer 0
               :input input})))



(defn run-file
  ([filename]
   (run-program (file-to-instructions filename)))
  ([filename input]
   (run-program (file-to-instructions filename) input)))


(defn run []
  (println "Day 05, part 1:")
  (run-file "src/aoc/day05/input.txt" 1)
  (println "Day 05, part 2:")
  (run-file "src/aoc/day05/input.txt" 5))