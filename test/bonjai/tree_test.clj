(ns bonjai.tree-test
  (:require [clojure.test :refer :all]
            [bonjai.tree :refer :all]))

(deftest test-build-map-matchers
  (is (= '([[:b 2] [:a 1]] [[:b y] [:a x]])
         (build-map-matchers {:a 1 :b 2 :c 3} {:a 'x :b 'y})))
  (is (= '([[1 :a] [2 :b]] [[1 x] [2 y]])
         (build-map-matchers {:a 1 :b 2 :c 3} {'x 1 'y 2})))
  (is (= '([[1 :a] [:b 2]] [[1 x] [:b y]])
         (build-map-matchers {:a 1 :b 2 :c 3} {'x 1 :b 'y})))
  (is (nil? (build-map-matchers {:a 1} {:a 'x :c 'y}))))

(deftest test-match
  (is (= {'a 1} (match 1 'a)))

  (is (= {'a 1} (match [1] '[a])))
  (is (not (match [1] [2])))

  (is (= {'a 1 'b 2} (match [1 2] '[a b])))
  (is (not (match [1 2] '[a b c])))
  (is (= {'b 2} (match [1 2] '[_ b])))
  (is (= {'b 2} (match [0 1 2] '[_ _ b])))
  (is (= {'a 0 'more [1 2]} (match [0 1 2] '[a & more])))
  (is (= {'x "deep"} (match [[["deep"]]] [[['x]]])))
  
  (is (= {'x 1} (match {:a 1} {:a 'x})))
  (is (= {'x 1} (match {:a 1 :b 2} {:a 'x})))
  (is (= {'x 1} (match {:a 1 :b 2 :c 3} {:a 'x})))
  (is (= {'x 1 'y 3} (match {:a 1 :b 2 :c 3} {:a 'x :c 'y})))
  (is (= {'x nil 'y nil} (match {:a nil :b nil :c nil} {:a 'x :c 'y})))
  (is (not (match {:a 1} {:a 'x :c 'y})))
  (is (= {'x :a 'y :c} (match {:a 1 :b 2 :c 3} {'x 1 'y 3})))
  (is (= {'x nil} (match {nil 1} {'x 1})))
  (is (= {'x 1 'y :c} (match {:a 1 :b 2 :c 3} {:a 'x 'y 3})))

  (is (= {'x 1 'y 66} (match {:a 1 :b [55 66 77] :c 3} {:a 'x :b ['_ 'y '_]}))))
