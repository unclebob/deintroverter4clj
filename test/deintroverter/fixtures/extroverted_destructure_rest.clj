(ns myapp.destructure-rest-test
  (:require [clojure.test :refer [deftest is]]
            [myapp.core :as core]))

(deftest destructures-with-rest
  (let [[head & tail] (core/split-items [1 2 3])]
    (is (= 1 head))))