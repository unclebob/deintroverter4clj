(ns myapp.core-test
  (:require [clojure.test :refer [deftest is]]
            [myapp.core :as core]))

(deftest destructures-result
  (let [[a b] (core/split-items [1 2])]
    (is (= 1 a))))