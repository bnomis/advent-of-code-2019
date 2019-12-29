(defproject advent-of-code "1.0.0"
  :description "Advent of Code 2019"
  :url "https://github.com/bnomis/advent-of-code-2019"
  :license {:name "MIT"
            :url "http://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "0.6.532"]
                 [org.clojure/math.combinatorics "0.1.6"]]

  :main ^:skip-aot aoc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
