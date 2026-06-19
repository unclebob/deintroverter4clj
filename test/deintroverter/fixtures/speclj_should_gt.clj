(ns myapp.should-gt-spec
  (:require [speclj.core :refer [describe it should>]]
            [myapp.core :as core]))

(describe "should>"
  (it "traces the compared value form"
    (should> (core/calculate-total [1 2]) 1)))