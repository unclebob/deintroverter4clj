(ns myapp.stamping-negative-spec
  (:require [speclj.core :refer [describe it should-not-contain]]))

(describe "should-not-contain"
  (it "recognizes negative key assertion"
    (let [stamped {:type :satellite :owner :player}]
      (should-not-contain :direction stamped))))