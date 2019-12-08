(ns aoc.core
  (:gen-class)
  (:require
    [aoc.day01.core]
    [aoc.day02.core]))


(defn run-all []
  (aoc.day01.core/run)
  (aoc.day02.core/run))


(defn run-day [day]
  (let [day (Integer/parseInt day)
        fmt (if (< day 10) "(aoc.day%02d.core/run)" "(aoc.day%d.core/run)")
        fn (format fmt day)]
    (eval (read-string fn))))


(defn -main
  [& args]
  (if (empty? args)
    (run-all)
    (mapv run-day args)))
