(ns myapp.destructure-nested-vectors-test
  (:require [clojure.test :refer [deftest is]]
            [myapp.core :as core]))

(deftest destructures-nested-vectors
  (let [[[a b] [c d]] (core/split-pairs [1 2 3 4])]
    (is (= 1 a))
    (is (= 4 d))))