(ns myapp.core-spec
  (:require [speclj.core :refer [describe it should=]]
            [myapp.core :as core]))

(describe "calculate-total"
  (it "returns count"
    (should= 2 (core/calculate-total [1 2]))))