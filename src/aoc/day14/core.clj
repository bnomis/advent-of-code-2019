(ns aoc.day14.core
  (:require
    [clojure.string :as str]))


(def state-init-1 {:recipes {:A {:output 10
                                 :inputs {:ORE 10}}
                             :B {:output 1
                                 :inputs {:ORE 1}}
                             :C {:output 1
                                 :inputs {:A 7
                                          :B 1}}
                             :D {:output 1
                                 :inputs {:A 7
                                          :C 1}}
                             :E {:output 1
                                 :inputs {:A 7
                                          :D 1}}
                             :FUEL {:output 1
                                    :inputs {:A 7
                                             :E 1}}}
                   :store {:A 0
                           :B 0
                           :C 0
                           :D 0
                           :E 0
                           :FUEL 0}
                   :ORE 0})



(def state-init-2 {:recipes {:A {:output 2
                                 :inputs {:ORE 9}}
                             :B {:output 3
                                 :inputs {:ORE 8}}
                             :C {:output 5
                                 :inputs {:ORE 7}}
                             :AB {:output 1
                                  :inputs {:A 3
                                           :B 4}}
                             :BC {:output 1
                                  :inputs {:B 5
                                           :C 7}}
                             :CA {:output 1
                                  :inputs {:A 1
                                           :C 4}}
                             :FUEL {:output 1
                                    :inputs {:AB 2
                                             :BC 3
                                             :CA 4}}}
                   :store {:A 0
                           :B 0
                           :C 0
                           :AB 0
                           :BC 0
                           :CA 0
                           :FUEL 0}
                   :ORE 0})


(def state-init {:recipes {}
                 :store {}
                 :ORE 0})


(declare get-ingredient)


(defn raise-to-power [x y]
  (reduce * (into [] (repeat y x))))


(defn in-stock [state ingredient quantity]
  (if (= :ORE ingredient)
    true
    (if-not (get-in state [:store ingredient])
      false
      (>= (get-in state [:store ingredient]) quantity))))


(defn take-stock [state ingredient quantity]
  (if (= :ORE ingredient)
    (update-in state [:ORE] + quantity)
    (update-in state [:store ingredient] - quantity)))


(defn make-recipe [state inputs]
  (reduce #(get-ingredient %1 %2 (get-in inputs [%2])) state (keys inputs)))


(defn make-recipe-batches [state inputs batches]
  (reduce
    #(get-ingredient %1 %2 (* (get-in inputs [%2]) batches))
    state
    (keys inputs)))


(defn amount-in-stock [state ingredient]
  (get-in state [:store ingredient]))


(defn add-ingredient [state ingredient quantity]
  (update-in state [:store ingredient] + quantity))


(defn make-ingredient [state ingredient]
  (let [recipe (get-in state [:recipes ingredient])
        inputs (get-in recipe [:inputs])
        unit (get-in recipe [:output])]
    (-> state
      (make-recipe inputs)
      (add-ingredient ingredient unit))))


(defn batch-size [state ingredient]
  (get-in state [:recipes ingredient :output]))


(defn make-ingredient-batches [state ingredient batches]
  (let [recipe (get-in state [:recipes ingredient])
        inputs (get-in recipe [:inputs])
        unit (get-in recipe [:output])]
    (-> state
        (make-recipe-batches inputs batches)
        (add-ingredient ingredient (* batches unit)))))


(defn make-stock-recursive [state ingredient quantity]
  (loop [state state]
    (if (>= (amount-in-stock state ingredient) quantity)
      state
      (recur (make-ingredient state ingredient)))))


(defn make-stock [state ingredient quantity]
  (let [gap (- quantity (amount-in-stock state ingredient))
        batches (int (Math/ceil (/ gap (batch-size state ingredient))))]
    (make-ingredient-batches state ingredient batches)))


(defn ensure-stock [state ingredient quantity]
  (if (in-stock state ingredient quantity)
    state
    (make-stock state ingredient quantity)))


(defn get-ingredient [state ingredient quantity]
  ;(println "get-ingredient" ingredient quantity)
  (-> state
    (ensure-stock ingredient quantity)
    (take-stock ingredient quantity)))


(defn ingredient-string-to-label-and-quantity [string]
  (let [parts (str/split (str/trim string) #" ")]
    [(keyword (get parts 1)) (Integer/parseInt (get parts 0))]))


(defn ingredients-string-to-inputs [string]
  (reduce
    (fn [c p] (conj c (ingredient-string-to-label-and-quantity p)))
    {}
    (str/split (str/trim string) #",")))


(defn line-to-recipe [line]
  (let [[inputs dest] (str/split (str/trim line) #"=>")
        dest (ingredient-string-to-label-and-quantity dest)
        inputs (ingredients-string-to-inputs inputs)]
    {(get dest 0) {:output (get dest 1)
                   :inputs inputs}}))


(defn lines-to-recipes [lines]
  (reduce #(conj %1 (line-to-recipe %2)) {} lines))


(defn string-to-lines [string]
  (str/split string #"\n"))


(defn file-to-string [filename]
  (str/trim (slurp filename)))


(defn file-to-lines [filename]
  (-> filename
    (file-to-string)
    (string-to-lines)))


(defn file-to-recipes [filename]
  (-> filename
    (file-to-lines)
    (lines-to-recipes)))


(defn recipes-to-empty-store [recipes]
  (reduce #(assoc-in %1 [%2] 0) {} (keys recipes)))


(defn file-to-state [filename]
  (let [recipes (file-to-recipes filename)]
    {:recipes recipes
     :store (recipes-to-empty-store recipes)
     :ORE 0}))


(defn state-to-ores-used [state]
  (:ORE state))


(defn state-to-unit-cost [state]
  (-> state
    (get-ingredient :FUEL 1)
    (state-to-ores-used)))


(defn file-to-one-fuel [filename]
  (let [state (file-to-state filename)
        state (get-ingredient state :FUEL 1)]
    (state-to-ores-used state)))


(defn print-ores-used [state]
  (println (:ORE state)))


(defn ore-quantity-to-fuels [state quantity fuels]
  (loop [state state
         fuels fuels]
    (let [ores-used (state-to-ores-used state)]
      ;(print-ores-used state)
      (if (= ores-used quantity)
        fuels
        (if (> ores-used quantity)
          (- fuels 1)
          (recur
            (get-ingredient state :FUEL 1)
            (inc fuels)))))))


(defn file-to-fuels-with-ore [filename quantity]
  (let [state (file-to-state filename)
        unit-cost (state-to-unit-cost state)
        fuels (int (/ quantity unit-cost))
        state (get-ingredient state :FUEL fuels)]
    (ore-quantity-to-fuels state quantity fuels)))


(defn print-state [state]
  (println state))


(defn test-get-ingredient []
  (-> state-init-2
    (get-ingredient :FUEL 1)
    (print-state)))


(defn run []
 (println "Day 14, part 1:" (file-to-one-fuel "src/aoc/day14/input.txt"))
 (println "Day 14, part 2:" (file-to-fuels-with-ore "src/aoc/day14/input.txt" 1000000000000)))