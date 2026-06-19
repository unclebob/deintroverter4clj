(ns myapp.defn-doc-test
  (:require [clojure.test :refer [deftest is]]
            [myapp.core :as core]))

(defn compute-total
  "Sums items via SUT."
  [items]
  (core/calculate-total items))

(deftest uses-docstring-defn-helper
  (let [result (compute-total [1 2])]
    (is (= 3 result))))