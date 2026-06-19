(ns myapp.fn-assertions-spec
  (:require [speclj.core :refer :all]
            [myapp.core :as core]))

(describe "fn literal assertions"
  (it "traces assertions inside invoked fn literals"
    (doseq [[input assertions]
            [[1 [#(should= 2 (core/calculate-total [% input]))]]]]
      (doseq [assertion assertions]
        (assertion input)))))