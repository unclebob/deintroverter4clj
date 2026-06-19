(ns myapp.destructure-symbol-keys-test
  (:require [clojure.test :refer [deftest is]]
            [myapp.core :as core]))

(deftest destructures-symbol-key-map
  (let [{a :a b :b} (core/labeled-pair 1 2)]
    (is (= 1 a))))