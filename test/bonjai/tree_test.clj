(ns bonjai.tree-test
  (:require [clojure.test :refer :all]
            [bonjai.tree :refer :all]))

(deftest test-match
  (is (= {'a 1} (match [1] '[a])))
  (is (= {'a 1 'b 2} (match [1 2] '[a b])))
  (is (= {'b 2} (match [1 2] '[_ b])))
  (is (= {'b 2} (match [0 1 2] '[_ _ b])))

  (is (= {'x 1} (match {:a 1} {:a 'x})))
  (is (= {'x 1} (match {:a 1 :b 2} {:a 'x})))
  (is (= {'x 1} (match {:a 1 :b 2 :c 3} {:a 'x})))
  (is (= {'x 1 'y 3} (match {:a 1 :b 2 :c 3} {:a 'x :c 'y})))

  (is (= {'x 1 'y 66} (match {:a 1 :b [55 66 77] :c 3} {:a 'x :b ['_ 'y '_]}))))
