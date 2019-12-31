(ns aoc.day08.core
  (:require
    [clojure.string :as str]))


(defn pixels-to-layers [pixels width height]
  (let [length (count pixels)
        pixels-per-layer (* width height)]
    (loop [start 0
           layers []]
      (if (= length start)
        layers
        (recur (+ start pixels-per-layer) (conj layers (subvec pixels start (+ start pixels-per-layer))))))))


(defn string-to-pixels [string]
  (into [] (map #(Integer/parseInt %1) (str/split string #""))))


(defn string-to-layers [string width height]
  (pixels-to-layers (string-to-pixels string) width height))


(defn file-to-string [filename]
  (str/trim (slurp filename)))


(defn file-to-layers [filename width height]
  (string-to-layers (file-to-string filename) width height))


(defn layer-count-digit [layer digit]
  (count (filterv #(= % digit) layer)))


(defn digit-to-keyword [digit]
  (keyword (str digit)))


(defn layer-count-to-digit-count [layer digit]
  ((digit-to-keyword digit) layer))


(defn layers-counts-to-digit-count [layers index digit]
  (layer-count-to-digit-count (layers index) digit))


(defn layer-to-counts [layer]
  (reduce #(assoc-in %1 [(digit-to-keyword %2)] (layer-count-digit layer %2)) {} layer))


(defn layers-to-counts [layers]
  (map #(layer-to-counts %1) layers))


(defn string-to-layer-counts [string width height]
  (into [] (layers-to-counts (string-to-layers string width height))))


(defn file-to-layer-counts [filename width height]
  (string-to-layer-counts (file-to-string filename) width height))


(defn layer-with-fewer-digits [layers digit current candidate]
  (if (< (layers-counts-to-digit-count layers candidate digit) (layers-counts-to-digit-count layers current digit))
    candidate
    current))


(defn layer-with-fewest [layers digit]
  (reduce #(layer-with-fewer-digits layers digit %1 %2) 0 (range (count layers))))


(defn file-to-layer-with-fewest [filename width height digit]
  (layer-with-fewest (file-to-layer-counts filename width height) digit))


(defn layer-multiply-digits [layer mul1 mul2]
  (* (layer-count-to-digit-count layer mul1) (layer-count-to-digit-count layer mul2)))


(defn file-to-layer-with-fewest-multiply [filename width height digit mul1 mul2]
  (let [layers (file-to-layer-counts filename width height)
        fewest (layer-with-fewest layers digit)]
    (layer-multiply-digits (layers fewest) mul1 mul2)))


(defn transparent-layer [width height]
  (into [] (repeat (* width height) 2)))


(defn add-pixel [dest src]
  (if (= dest 2)
    src
    dest))


(defn add-layer [dest src]
  (let [last (count dest)]
    (loop [out []
           index 0]
      (if (= index last)
        out
        (recur (conj out (add-pixel (dest index) (src index))) (inc index))))))


(defn render-layers [layers width height]
  (reduce add-layer (transparent-layer width height) layers))


(defn render-string [string width height]
  (render-layers (string-to-layers string width height) width height))


(defn render-file [filename width height]
  (render-string (file-to-string filename) width height))


(defn print-layer [layer width height]
  (let [last (count layer)]
    (loop [index 0]
      (if (< index last)
        (do
          (println (subvec layer index (+ index width)))
          (recur (+ index width)))))))


(defn print-file [filename width height]
  (print-layer (render-file filename width height) width height))


(defn run []
  (println "Day 08, part 1:" (file-to-layer-with-fewest-multiply "src/aoc/day08/input.txt" 25 6 0 1 2))
  (println "Day 08, part 2:")
  (print-file "src/aoc/day08/input.txt" 25 6))
