(ns aoc.core
  (:gen-class)
  (:require
    [aoc.day01.core]
    [aoc.day02.core]
    [aoc.day03.core]
    [aoc.day04.core]
    [aoc.day05.core]
    [aoc.day06.core]
    [aoc.day07.core]
    [aoc.day08.core]))


(defn run-all []
  (aoc.day01.core/run)
  (aoc.day02.core/run)
  (aoc.day03.core/run)
  (aoc.day04.core/run)
  (aoc.day05.core/run)
  (aoc.day06.core/run)
  (aoc.day07.core/run)
  (aoc.day08.core/run))


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
