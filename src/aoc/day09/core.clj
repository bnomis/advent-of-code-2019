(ns aoc.day09.core
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [clojure.core.async :as async]))


(def state-init {:memory []
                 :pointer 0
                 :relative-base 0
                 :phase nil
                 :inputs []
                 :input-channel nil
                 :outputs []
                 :output-channel nil})


(defn add-instructions [state instructions]
  (assoc-in state [:memory] instructions))


(defn add-input [state value]
  (-> state
    (assoc-in [:phase] nil)
    (update-in [:inputs] conj value)))


(defn add-input-channel [state channel]
  (assoc-in state [:input-channel] channel))


(defn read-input [state]
  (if (:phase state)
    (:phase state)
    (async/<!! (:input-channel state))))


(defn send-input [state value]
  (async/>!! (:input-channel state) value))


(defn add-output [state value]
  (update-in state [:outputs] conj value))


(defn write-output [state value]
  (async/>!! (:output-channel state) value)
  (add-output state value))


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
          (do
            (get-last-output state))
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


(defn run-string-with-input [string value]
  (run-amp-with-input (make-amp (string-to-instructions string) (make-channel) (make-channel)) value))


(defn run-file-with-input [filename value]
  (run-string-with-input (file-to-string filename) value))


(defn run []
  (println "Day 09, part 1:" (run-file-with-input "src/aoc/day09/input.txt" 1))
  (println "Day 09, part 2:" (run-file-with-input "src/aoc/day09/input.txt" 2)))
