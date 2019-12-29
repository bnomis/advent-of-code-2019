(ns aoc.day07.core
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [clojure.core.async :as async]))


(def state-init {:memory []
                 :pointer 0
                 :phase nil
                 :inputs []
                 :input-channel nil
                 :outputs []
                 :output-channel nil})


(defn add-instructions [state instructions]
  (assoc-in state [:memory] instructions))


(defn add-phase [state phase]
  (assoc-in state [:phase] phase))


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


(defn input-op [state]
  (let [value (read-input state)
        dest (get-value-relative-to-pointer state 1)]
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
  (let [phase (get-in state [:phase])]
    (loop [state state]
      (let [op (get-op state)]
        (if (= 99 op)
          (do
            (get-last-output state))
          (recur (exec-op state op)))))))


(defn make-amp [instructions phase input-channel output-channel]
  (-> state-init
    (add-instructions instructions)
    (add-phase phase)
    (add-input-channel input-channel)
    (add-output-channel output-channel)))


(defn run-amp [amp]
  (async/thread (run-state amp)))


(defn run-amps [amps]
  (let [runners (mapv run-amp amps)]
    (send-input (first amps) 0)
    (async/<!! (last runners))))


(defn input-channel [channels phase]
  (channels phase))


(defn output-channel [channels phase]
  (let [last (- (count channels) 1)
        index (+ phase 1)]
    (if (> index last)
      (channels 0)
      (channels index))))


(defn make-channel []
  (async/chan 10))


(defn make-amp-phases [instructions phases]
  (let [last (count phases)
        channels (into [] (repeatedly last make-channel))]
    (loop [phase 0
           amps []]
      (if (= phase last)
        amps
        (recur (inc phase) (conj amps (make-amp instructions (phases phase) (input-channel channels phase) (output-channel channels phase))))))))


(defn make-and-run-amp-phases [instructions phases]
  (run-amps (make-amp-phases instructions phases)))


(defn run-phase-combinations [instructions combinations]
  (map #(make-and-run-amp-phases instructions %1) combinations))


(defn find-max-thrust [instructions combinations]
  (last (sort (run-phase-combinations instructions combinations))))


(defn make-permutations [phases]
  (combo/permutations phases))


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


(defn file-to-max-thrust [filename phases]
  (let [instructions (file-to-instructions filename)
        combinations (make-permutations phases)]
    (find-max-thrust instructions combinations)))


(defn run []
  (println "Day 07, part 1:" (file-to-max-thrust "src/aoc/day07/input.txt" [0 1 2 3 4]))
  (println "Day 07, part 2:" (file-to-max-thrust "src/aoc/day07/input.txt" [5 6 7 8 9])))
