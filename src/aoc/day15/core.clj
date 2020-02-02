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


(defn update-x [c x]
  (if (< x (:min-x c))
    (assoc-in c [:min-x] x)
    (if (> x (:max-x c))
      (assoc-in c [:max-x] x)
      c)))


(defn update-y [c y]
  (if (< y (:min-y c))
    (assoc-in c [:min-y] y)
    (if (> y (:max-y c))
      (assoc-in c [:max-y] y)
      c)))


(defn map-to-min-max [map]
  (reduce
    (fn [c kw]
      (let [pos (keyword-to-position kw)
            x (get pos 0)
            y (get pos 1)]
        (-> c
          (update-x x)
          (update-y y))))
    {:min-x 0
     :max-x 0
     :min-y 0
     :max-y 0}
    (keys map)))


(defn empty-row [columns]
  (into [] (take columns (repeat "="))))


(defn min-max-to-grid [{:keys [min-x max-x min-y max-y]}]
  (let [columns (+ 1 (- max-x min-x))
        rows (+ 1 (- max-y min-y))]
    (loop [rows rows
           grid []]
      (if (= 0 rows)
        grid
        (recur (dec rows) (conj grid (empty-row columns)))))))


(defn value-to-grid [value]
  (if (int? value)
    "."
    value))


(defn set-grid-value [grid row column value]
  ;(println "set-grid-value" row column value)
  (assoc-in grid [row column] (value-to-grid value)))


(defn position-to-grid [x min-x]
  (+ x (Math/abs min-x)))


(defn position-to-row-column [[x y] min-x min-y]
  [(position-to-grid y min-y) (position-to-grid x min-x)])


(defn keyword-to-row-column [kw min-x min-y]
  (position-to-row-column (keyword-to-position kw) min-x min-y))


(defn print-grid [grid]
  (let [rows (count grid)]
    (loop [index 0]
      (if (< index rows)
        (do
          (println (get grid index))
          (recur (inc index)))))))


(defn map-to-grid [map grid {:keys [min-x min-y]}]
  (reduce
    (fn [g kw]
      ;(println kw)
      ;(print-grid g)
      (let [[row column] (keyword-to-row-column kw min-x min-y)]
        (set-grid-value g row column (get-in map [kw]))))
    grid
    (keys map)))


(defn set-grid-at-position [grid position min-x min-y value]
  (let [[row column] (position-to-row-column position min-x min-y)]
    (assoc-in grid [row column] value)))


(defn robot-to-grid [state grid {:keys [min-x min-y]}]
  (-> grid
    (set-grid-at-position (:robot state) min-x min-y "X")
    (set-grid-at-position (:oxygen state) min-x min-y "O")))


(defn width [{:keys [min-x max-x]}]
  (+ 1 (- max-x min-x)))


(defn height [{:keys [min-y max-y]}]
  (+ 1 (- max-y min-y)))


(defn visualise-map [state]
  (let [min-max (map-to-min-max (:map state))
        grid (min-max-to-grid min-max)
        mapped-grid (map-to-grid (:map state) grid min-max)
        robot-grid (robot-to-grid state mapped-grid min-max)]
    (print-grid robot-grid)
    (println (width min-max) "x" (height min-max))))


(defn add-wall [state x y]
  (assoc-in state [:map (position-to-keyword [x y])] "#"))


(defn wall-row [state row min-x max-x]
  (loop [state state
         x min-x]
    (if (> x max-x)
      state
      (recur (add-wall state x row) (inc x)))))


(defn wall-column [state column min-y max-y]
  (loop [state state
         y min-y]
    (if (> y max-y)
      state
      (recur (add-wall state column y) (inc y)))))


(defn edge-map [state]
  (let [{:keys [min-x max-x min-y max-y]} (map-to-min-max (:map state))]
    (-> state
      (wall-row min-y min-x max-x)
      (wall-row max-y min-x max-x)
      (wall-column min-x min-y max-y)
      (wall-column max-x min-y max-y))))


(defn unvisited-position? [state position]
  (not (contains? (:map state) (position-to-keyword position))))


(defn corner-position? [[x y] min-x max-x min-y max-y]
  (or
    (and (= x min-x) (= y min-y))
    (and (= x min-x) (= y max-y))
    (and (= x max-x) (= y min-y))
    (and (= x max-x) (= y max-y))))


(defn not-corner-position? [[x y] min-x max-x min-y max-y]
  (not (corner-position? [x y] min-x max-x min-y max-y)))


(defn unvisited-not-corner-position? [state position min-x max-x min-y max-y]
  (and
    (unvisited-position? state position)
    (not-corner-position? position min-x max-x min-y max-y)))


(defn unvisited-positions [state]
  (let [{:keys [min-x max-x min-y max-y]} (map-to-min-max (:map state))]
    (for [x (range min-x (+ 1 max-x))
          y (range min-y (+ 1 max-y))
          :let [position [x y]]
          :when (unvisited-not-corner-position? state position min-x max-x min-y max-y)]
      position)))


(defn seeker-state [cpu]
  {:robot [0 0]
   :oxygen [0 0]
   :map {(position-to-keyword [0 0]) 1}
   :statuses []
   :cmd-channel (:input-channel cpu)
   :status-channel (:output-channel cpu)
   :edges #{}
   :oxygen-full []
   :seen-nodes #{}})


(defn null-vector? [[x y]]
  (and (= 0 x) (= 0 y)))


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


(defn set-oxygen-position [state new-position]
  (assoc-in state [:oxygen] new-position))


(defn set-robot-position [state new-position]
  (assoc-in state [:robot] new-position))


(defn robot-position-keyword [state]
  (position-to-keyword (:robot state)))


(defn update-robot-position [state d]
  (let [new-position (robot-position state d)]
    ;(println "update-robot-position" new-position)
    (-> state
        (add-edge (robot-position-keyword state)
                  (position-to-keyword new-position))
        (set-robot-position new-position))))


(defn hit-wall [state d]
  ;(println "hit-wall")
  (let [wall-position (robot-position state d)]
    (-> state
        (add-to-map wall-position "#")
        (append-status 0))))


(defn moved [state d]
  ;(println "moved")
  (let [new-position (robot-position state d)]
    (-> state
      (increase-position-count new-position)
      (append-status 1)
      (update-robot-position d))))


(defn found [state d]
  ;(println "found!")
  (let [new-position (robot-position state d)]
    (-> state
        (set-oxygen-position new-position)
        (increase-position-count new-position)
        (append-status 2)
        (update-robot-position d))))


(defn move [state d]
  ;(println "move" d)
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


(defn opposite-direction [d]
  (case d
    1 2
    2 1
    3 4
    4 3))


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


(defn keyword-is-a-wall [state kw]
  (= "#" (get-in state [:map kw])))


(defn keyword-is-not-a-wall [state kw]
  (not (keyword-is-a-wall state kw)))


(defn keyword-is-visitable [state kw]
  ;(println "keyword-is-visitable" kw)
  (if (and (contains? (:map state) kw) (keyword-is-not-a-wall state kw))
    true
    false))


(defn position-is-visitable [state position]
  ;(println "position-is-visitable" position)
  (keyword-is-visitable state (position-to-keyword position)))


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
  ;(println "counts" counts)
  (sort #(compare (last %1) (last %2)) counts))


(defn find-least-visited-direction [state]
  (let [sorted (sort-visit-counts (visit-counts state))]
    ;(println "find-least-visited-direction" sorted)
    (first (first sorted))))


(defn take-steps [state direction steps]
  ;(println "take-steps" direction steps)
  (loop [state (move state direction)
         count (- steps 1)]
    (if (or (= 0 count) (last-status-was-wall state) (last-status-was-oxygen state))
      state
      (recur (move state direction) (- count 1)))))


(defn move-until-wall [state d]
  (let [state (move state d)]
    (loop [state state]
      (if (last-status-was-wall state)
        state
        (recur (move state d))))))


(defn move-until-position [state d position]
  (loop [state state]
    (if (= position (:robot state))
      state
      (recur (move state d)))))


(defn probe-direction [state d]
  (let [start (:robot state)
        state (move-until-wall state d)]
    (move-until-position state (opposite-direction d) start)))


(defn probe [state]
  (reduce
    (fn [s d] (probe-direction s d))
    state
    all-directions))


(defn do-seek [state]
  (let [unvisited-direction (find-unvisited-direction state)]
    (if unvisited-direction
      (move state unvisited-direction)
      (move state (find-least-visited-direction state)))))


(defn oxygen-kw [state]
  (position-to-keyword (:oxygen state)))


(defn oxygen-position [state]
  (:oxygen state))


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


(defn move-delta [state delta]
  (move state (vector-to-direction delta)))


(defn keyword-delta [frm to]
  (let [[frm-x frm-y] (keyword-to-position frm)
        [to-x to-y] (keyword-to-position to)]
    [(- to-x frm-x) (- to-y frm-y)]))


(defn move-to-keyword [state to]
  (let [frm (robot-position-keyword state)]
    (if (= frm to)
      state
      (move-delta state (keyword-delta frm to)))))


(defn walk-path [state path]
  ;(println "walk-path" path)
  (reduce move-to-keyword state path))


(defn move-to-position [state [to-x to-y]]
  (let [[frm-x frm-y] (:robot state)
        delta [(- to-x frm-x) (- to-y frm-y)]]
    (move-delta state delta)))


(defn has-path? [state to]
  (let [frm (robot-position-keyword state)
        to-kw (position-to-keyword to)
        g (apply graph/graph (:edges state))
        path (alg/bf-path g frm to-kw)]
    (if path
      true
      false)))


(def neighbour-vectors
  [[1 0]
   [-1 0]
   [0 1]
   [0 -1]])


(defn state-to-graph [state]
  (apply graph/graph (:edges state)))


(defn path-between-keywords [graph frm to]
  (alg/bf-path graph frm to))


(defn path-between-keywords? [graph frm to]
  (if (path-between-keywords graph frm to)
    true
    false))


(defn path-between-positions [graph frm to]
  (path-between-keywords graph (position-to-keyword frm) (position-to-keyword to)))


(defn position-to-neighbours [position]
  (reduce
    (fn [c p]
      (conj c (add-vectors position p)))
    []
    neighbour-vectors))


(defn position-to-visitable-neighbours [state position]
  (filterv
    (fn [p]
      (position-is-visitable state p))
    (position-to-neighbours position)))


(defn first-path-to-positions [state positions]
  (let [position-count (count positions)
        g (state-to-graph state)
        frm (:robot state)]
    (loop [index 0]
      (if (< index position-count)
        (let [path (path-between-positions g frm (get positions index))]
          (if path
            path
            (recur (inc index))))))))


(defn path-to-position-neighbours [state position]
  (let [neighbours (position-to-visitable-neighbours state position)
        neighbour-count (count neighbours)]
    ;(println "neighbours" neighbours)
    (if (= neighbour-count 0)
      nil
      (first-path-to-positions state neighbours))))


(defn can-visit-position? [state position]
  ;(println "can-visit-position?" position)
  (if (path-to-position-neighbours state position)
    true
    false))


(defn visitable-unvisited-positions [state]
  (filterv
    (fn [p]
      (can-visit-position? state p))
    (unvisited-positions state)))


(defn visit-position [state position]
  ;(println "visit-position" position)
  (let [path (path-to-position-neighbours state position)
        state (walk-path state path)]
    (-> state
      (move-to-position position)
      (probe))))


(defn walk-to [state to]
  (let [frm (robot-position-keyword state)
        to-kw (position-to-keyword to)
        g (apply graph/graph (:edges state))
        path (alg/bf-path g frm to-kw)]
    ;(println "walk-to" frm to-kw)
    (if path
      (walk-path state path)
      state)))


(defn walk [state unvisited]
  ;(println "walk unvisited" (count unvisited))
  (reduce walk-to state unvisited))


(defn visit-unvisited [state]
  (let [unvisited (unvisited-positions state)]
    (visualise-map state)
    (if-not unvisited
      state
      (visit-unvisited (walk state unvisited)))))


(defn init-oxygen-full [state]
  (let [kw (position-to-keyword (:oxygen state))]
    (-> state
      (assoc-in [:oxygen-full] [[kw]])
      (assoc-in [:seen-nodes] #{kw}))))


(defn seen-node? [state node]
  (contains? (:seen-nodes state) node))


(defn unseen-node? [state node]
  (not (seen-node? state node)))


(defn filter-out-seen-nodes [state nodes]
  (filterv
    (fn [n]
      (unseen-node? state n))
    nodes))


(defn add-to-seen-nodes [state nodes]
  (update-in state [:seen-nodes] into nodes))


(defn count-oxygen-full-nodes [state]
  (reduce + (map count (:oxygen-full state))))


(defn count-oxygen-full-loops [state]
  (- (count (:oxygen-full state)) 1))


(defn node-to-successors [g n]
  (graph/successors g n))


(defn nodes-to-successors [g nodes]
  (reduce
    (fn [c n]
      (into c (node-to-successors g n)))
    []
    nodes))


(defn append-oxygen-full [state nodes]
  (update-in state [:oxygen-full] conj nodes))


(defn last-oxygen-full [state]
  (last (:oxygen-full state)))


(defn neighbour-fill [state g]
  (let [nodes (last-oxygen-full state)
        successors (nodes-to-successors g nodes)
        unseen-nodes (filter-out-seen-nodes state successors)]
    (-> state
      (append-oxygen-full unseen-nodes)
      (add-to-seen-nodes unseen-nodes))))


(defn oxygen-fill [state]
  ;(println "oxygen-fill")
  (let [g (state-to-graph state)
        state (init-oxygen-full state)
        node-count (count (graph/nodes g))]
    (loop [state state]
      ;(println "filled" (:oxygen-full state))
      (if (= node-count (count-oxygen-full-nodes state))
        state
        (recur (neighbour-fill state g))))))


(defn oxygen-fill-count [state]
  (let [state (oxygen-fill state)]
    (count-oxygen-full-loops state)))


(defn visited-all [state]
  (= (count (unvisited-positions state)) 0))


(defn visit-positions [state positions]
  (reduce
    (fn [s p]
      (visit-position s p))
    state
    positions))


(defn explore-map [state]
  ;(println "explore-map")
  ;(visualise-map state)
  ;(println "")
  (let [positions (visitable-unvisited-positions state)
        count (count positions)]
    ;(println positions)
    (if (= 0 count)
      state
      (explore-map (visit-positions state positions)))))


(defn file-to-oxygen-fill [filename]
  (let [cpu (file-to-cpu filename)
        cpu-thread (run-cpu-in-thread cpu)
        state (seek-oxygen cpu)
        state (explore-map state)]
    (oxygen-fill-count state)))


(defn run []
  (println "Day 15, part 1:" (file-to-oxygen-distance "src/aoc/day15/input.txt"))
  (println "Day 15, part 2:" (file-to-oxygen-fill "src/aoc/day15/input.txt")))