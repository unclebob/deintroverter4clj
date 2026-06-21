(ns myapp.assertion-sut-invoke-spec
  (:require [speclj.core :refer [describe it should= should-contain]]
            [myapp.core :as core]
            [myapp.debug :as debug]))

(describe "sut invoke in assertion"
  (it "classifies a direct sut call in should= as extroverted"
    (should= 1 (core/calculate-total [1])))

  (it "promotes a namespaced production invoke not in the inferred sut set"
    (should-contain "pos" (debug/format-cell "pos" "cell")))

  (it "stays introverted when the asserted form is not a production invoke"
    (should= 3 (count [1 2 3]))))