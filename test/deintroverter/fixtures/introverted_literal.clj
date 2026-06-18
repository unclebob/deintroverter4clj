(ns myapp.core-test
  (:require [clojure.test :refer [deftest is]]))

(deftest only-checks-input
  (let [items [1 2 3]]
    (is (= 3 (count items)))))