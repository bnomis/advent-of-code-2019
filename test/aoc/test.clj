(ns aoc.test
  (:require [clojure.test :refer [deftest testing is]]
            [aoc.day01.core :refer [mass-to-fuel mass-to-fuel2]]))


(deftest day-01-part-1
  (testing "mass-to-fuel"
    (is (= 2 (mass-to-fuel 12)))
    (is (= 2 (mass-to-fuel 14)))
    (is (= 654 (mass-to-fuel 1969)))
    (is (= 33583 (mass-to-fuel 100756)))))


(deftest day-01-part-2
  (testing "mass-to-fuel2"
    (is (= 2 (mass-to-fuel2 14)))
    (is (= 966 (mass-to-fuel2 1969)))
    (is (= 50346 (mass-to-fuel2 100756)))))
