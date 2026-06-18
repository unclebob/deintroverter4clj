(ns myapp.core-test
  (:require [clojure.test :refer [deftest is]]
            [myapp.core :as core]))

(deftest calculates-total
  (is (= 2 (core/calculate-total [1 2]))))