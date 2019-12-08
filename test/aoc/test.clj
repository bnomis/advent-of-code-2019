(ns aoc.test
  (:require [clojure.test :refer [deftest testing is]]
            [aoc.day01.core :refer [mass-to-fuel mass-to-fuel2]]
            [aoc.day02.core :refer [run-program]]))


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


(deftest day-02-part-1
  (testing "run-program"
    (is (= [3500,9,10,70,2,3,11,0,99,30,40,50] (run-program [1,9,10,3,2,3,11,0,99,30,40,50])))
    (is (= [2,0,0,0,99] (run-program [1,0,0,0,99])))
    (is (= [2,3,0,6,99] (run-program [2,3,0,3,99])))
    (is (= [2,4,4,5,99,9801] (run-program [2,4,4,5,99,0])))
    (is (= [30,1,1,4,2,5,6,0,99] (run-program [1,1,1,4,99,5,6,0,99])))))
