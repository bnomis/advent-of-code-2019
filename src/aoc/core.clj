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
    [aoc.day08.core]
    [aoc.day09.core]
    [aoc.day10.core]
    [aoc.day11.core]
    [aoc.day12.core]
    [aoc.day13.core]
    [aoc.day14.core]
    [aoc.day15.core]
    [aoc.day16.core]
    [aoc.day17.core]
    [aoc.day18.core]
    [aoc.day19.core]
    [aoc.day20.core]
    [aoc.day21.core]
    [aoc.day22.core]
    [aoc.day23.core]
    [aoc.day24.core]
    [aoc.day25.core]))


(defn run-all []
  (aoc.day01.core/run)
  (aoc.day02.core/run)
  (aoc.day03.core/run)
  (aoc.day04.core/run)
  (aoc.day05.core/run)
  (aoc.day06.core/run)
  (aoc.day07.core/run)
  (aoc.day08.core/run)
  (aoc.day09.core/run)
  (aoc.day10.core/run)
  (aoc.day11.core/run)
  (aoc.day12.core/run)
  (aoc.day13.core/run)
  (aoc.day14.core/run)
  (aoc.day15.core/run)
  (aoc.day16.core/run)
  (aoc.day17.core/run)
  (aoc.day18.core/run)
  (aoc.day19.core/run)
  (aoc.day20.core/run)
  (aoc.day21.core/run)
  (aoc.day22.core/run)
  (aoc.day23.core/run)
  (aoc.day24.core/run)
  (aoc.day25.core/run))


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
