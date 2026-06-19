(ns myapp.destructure-keys-test
  (:require [clojure.test :refer [deftest is]]
            [myapp.core :as core]))

(deftest destructures-map-keys
  (let [{:keys [a b]} (core/labeled-pair 1 2)]
    (is (= 1 a))))