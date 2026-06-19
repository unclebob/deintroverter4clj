(ns myapp.spec-mother-spec
  (:require [speclj.core :refer [describe it should=]]
            [myapp.spec-mother :refer [valid-input?]]))

(describe "spec mother"
  (it "asserts via spec helper without reaching SUT"
    (should= true (valid-input? 1))))