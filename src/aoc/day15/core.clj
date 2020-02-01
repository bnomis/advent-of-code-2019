(ns aoc.day15.core
  (:require
    [clojure.string :as str]
    [clojure.core.async :as async]
    [loom.graph :as graph]
    [loom.alg :as alg]))


(def state-init {:memory []
                 :pointer 0
                 :relative-base 0
                 :inputs []
                 :input-channel nil
                 :outputs []
                 :output-channel nil})


(defn add-instructions [state instructions]
  (assoc-in state [:memory] instructions))


(defn set-memory [state address value]
  (assoc-in state [:memory address] value))


(defn add-input [state value]
  (-> state
      (update-in [:inputs] conj value)))


(defn add-output [state value]
  (update-in state [:outputs] conj value))


(defn add-input-channel [state channel]
  (assoc-in state [:input-channel] channel))


(defn add-output-channel [state channel]
  (assoc-in state [:output-channel] channel))


(defn read-input [state]
  (async/<!! (:input-channel state)))


(defn write-output [state value]
  (async/>!! (:output-channel state) value))


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
        value (get-value-relative-to-pointer-with-mode state 1 mode1)
        wrote (write-output state value)]
    (-> state
        (add-output value)
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
  (println "pointer:" (:pointer state)
           "op:" (get-value-at state (:pointer state))
           "relative-base:" (:relative-base state)
           "inputs:" (:inputs state)
           "input-channel:" (:input-channel state)
           "output-channel:" (:output-channel state)))


(defn run-cpu [state]
  (loop [state state]
    ;(print-state state)
    (let [op (get-op state)]
      (if (= 99 op)
        state
        (recur (exec-op state op))))))


(defn run-cpu-in-thread [cpu]
  (async/thread (run-cpu cpu)))


(defn make-cpu [instructions input-channel output-channel]
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


(defn string-to-cpu [string]
  (make-cpu (string-to-instructions string) (make-channel) (make-channel)))


(defn file-to-cpu [filename]
  (string-to-cpu (file-to-string filename)))


(defn run-string [string]
  (run-cpu (string-to-cpu string)))


(defn run-string-in-thread [string]
  (run-cpu-in-thread (string-to-cpu string)))


(defn run-file [filename]
  (run-string (file-to-string filename)))


(defn run-file-in-thread [filename]
  (run-string-in-thread (file-to-string filename)))


(defn last-status [state]
  (last (:statuses state)))


(defn last-status-was-oxygen [state]
  (= 2 (last-status state)))


(defn last-status-was-wall [state]
  (= 0 (last-status state)))


(defn write-cmd [state cmd]
  ;(println "write-cmd" state cmd)
  (async/>!! (:cmd-channel state) cmd))


(defn read-status [state]
  ;(println "read-status")
  (async/<!! (:status-channel state)))


(defn position-to-keyword [[x y]]
  (keyword (str (str x) "," (str y))))


(defn keyword-to-position [kw]
  (into [] (map #(Integer/parseInt %1) (str/split (name kw) #","))))


(defn seeker-state [cpu]
  {:robot [0 0]
   :map {(position-to-keyword [0 0]) 1}
   :statuses []
   :cmd-channel (:input-channel cpu)
   :status-channel (:output-channel cpu)
   :edges #{}})


(defn vector-to-direction [[x y]]
  (if (= 0 x)
    (if (> y 0)
      2
      1)
    (if (> x 0)
      4
      3)))


(defn direction-to-vector [d]
  (case d
    1 [0 -1]
    2 [0 1]
    3 [-1 0]
    4 [1 0]))


(defn add-vectors [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])


(defn add-to-map [state position value]
  (assoc-in state [:map (position-to-keyword position)] value))


(defn increase-position-count [state position]
  (let [kw (position-to-keyword position)]
    (if (contains? (:map state) kw)
      (update-in state [:map kw] inc)
      (assoc-in state [:map kw] 1))))


(defn append-status [state status]
  (update-in state [:statuses] conj status))


(defn robot-position [state d]
  (add-vectors (:robot state) (direction-to-vector d)))


(defn step-vector [vector steps]
  [(* steps (get vector 0)) (* steps (get vector 1))])


(defn step-robot-position [state steps d]
  (add-vectors (:robot state) (step-vector (direction-to-vector d) steps)))


(defn add-edge [state frm to]
  (update-in state [:edges] conj [frm to]))


(defn set-robot-position [state new-position]
  (assoc-in state [:robot] new-position))


(defn robot-position-keyword [state]
  (position-to-keyword (:robot state)))


(defn update-robot-position [state d]
  (let [new-position (robot-position state d)]
    (println "update-robot-position" new-position)
    (-> state
        (add-edge (robot-position-keyword state) (position-to-keyword new-position))
        (set-robot-position new-position))))


(defn hit-wall [state d]
  (println "hit-wall")
  (let [wall-position (robot-position state d)]
    (-> state
        (add-to-map wall-position "#")
        (append-status 0))))


(defn moved [state d]
  (println "moved")
  (let [new-position (robot-position state d)]
    (-> state
      (increase-position-count new-position)
      (append-status 1)
      (update-robot-position d))))


(defn found [state d]
  (println "found!")
  (let [new-position (robot-position state d)]
    (-> state
      (add-to-map new-position "O")
      (append-status 2)
      (update-robot-position d))))


(defn move [state d]
  (println "move" d)
  (write-cmd state d)
  (case (read-status state)
    0 (hit-wall state d)
    1 (moved state d)
    2 (found state d)))


(defn random-direction []
  (+ 1 (rand-int 4)))


(defn random-steps []
  (+ 2 (rand-int 2)))


(defn next-direction [d]
  (case d
    1 2
    2 3
    3 4
    4 1))


(defn isa [state position value]
  (= value (get-in state [:map (position-to-keyword position)])))


(defn visited? [state position]
  (contains? (:map state) (position-to-keyword position)))


(defn wall? [state position]
  (and (visited? state position) (isa state position "#")))


(defn is-not-a-wall [state steps direction]
  (not (wall? state (step-robot-position state steps direction))))


(defn filter-out-walls [state steps directions]
  (filterv #(is-not-a-wall state steps %1) directions))


(defn not-visited [state steps direction]
  (not (visited? state (step-robot-position state steps direction))))


(defn filter-out-visited [state steps directions]
  (filterv #(not-visited state steps %1) directions))


(def all-directions
  [1 2 3 4])


(defn filter-directions [state steps]
  (filter-out-visited state steps (filter-out-walls state steps all-directions)))


(defn find-direction [state]
  (let [no-walls (filter-out-walls state 1 all-directions)
        unvisited (filter-out-visited state 1 no-walls)
        len-unvisited (count unvisited)]
    (if (= 0 len-unvisited)
      (first (shuffle no-walls))
      (first (shuffle unvisited)))))


(defn find-unvisited-direction [state]
  (let [no-walls (filter-out-walls state 1 all-directions)
        unvisited (filter-out-visited state 1 no-walls)
        len-unvisited (count unvisited)]
    (if (> len-unvisited 0)
      (first (shuffle unvisited)))))


(defn find-unwalled-direction [state]
  (let [no-walls (filter-out-walls state 1 all-directions)]
      (first (shuffle no-walls))))


(defn count-visits [state direction]
  (let [kw (position-to-keyword (robot-position state direction))]
    (if (contains? (:map state) kw)
      (get-in state [:map kw])
      0)))


(defn visit-counts [state]
  (reduce
    (fn [c d] (conj c [d (count-visits state d)]))
    [] (filter-out-walls state 1 all-directions)))


(defn sort-visit-counts [counts]
  (println "counts" counts)
  (sort #(compare (last %1) (last %2)) counts))


(defn find-least-visited-direction [state]
  (let [sorted (sort-visit-counts (visit-counts state))]
    (println "sorted" sorted)
    (first (first sorted))))


(defn take-steps [state direction steps]
  (println "take-steps" direction steps)
  (loop [state (move state direction)
         count (- steps 1)]
    (if (or (= 0 count) (last-status-was-wall state) (last-status-was-oxygen state))
      state
      (recur (move state direction) (- count 1)))))


(defn do-seek [state]
  (let [unvisited-direction (find-unvisited-direction state)]
    (if unvisited-direction
      (move state unvisited-direction)
      (move state (find-least-visited-direction state)))))


(defn oxygen-kw [state]
  (let [kws (keys (:map state))]
    (loop [kw (first kws)
           kws (rest kws)]
      (if (= "O" (get-in state [:map kw]))
        kw
        (recur (first kws) (rest kws))))))


(defn oxygen-position [state]
  (keyword-to-position (oxygen-kw state)))


(defn oxygen-distance-old [state]
  (let [pos (oxygen-position state)]
    (+ (Math/abs (get pos 0)) (Math/abs (get pos 1)))))


(defn oxygen-distance [state]
  (let [frm (position-to-keyword [0 0])
        to (oxygen-kw state)
        g (apply graph/graph (:edges state))
        path (alg/shortest-path g frm to)]
    (- (count path) 1)))


(defn print-seek-state [state]
  (println "robot" (:robot state))
  (println "edges" (:edges state)))


(defn seek-oxygen [cpu]
  (loop [state (seeker-state cpu)]
    ;(print-seek-state state)
    (if (last-status-was-oxygen state)
      state
      (recur (do-seek state)))))


(defn file-to-oxygen-distance [filename]
  (let [cpu (file-to-cpu filename)
        cpu-thread (run-cpu-in-thread cpu)
        state (seek-oxygen cpu)]
    (oxygen-distance state)))


(defn run []
  (println "Day 15, part 1:" (file-to-oxygen-distance "src/aoc/day15/input.txt"))
  (println "Day 15, part 2:"))