(ns myapp.nested-destructure-test
  (:require [clojure.test :refer [deftest is]]
            [myapp.core :as core]))

(deftest nested-destructure
  (let [[a [b c]] (core/split-items [1 2 3])]
    (is (= 1 a))))