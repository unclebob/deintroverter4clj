(ns myapp.binding-output-spec
  (:require [speclj.core :refer [describe it should= should]]
            [myapp.core :as core]))

(describe "binding output capture"
  (it "walks assertions inside binding after sut call"
    (let [out (java.io.StringWriter.)]
      (binding [*out* out]
        (core/calculate-total [1 2])
        (should= "" (str out)))))

  (it "walks assertions inside binding with nested let"
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (let [total (core/calculate-total [1 2 3])]
          (should= 3 total)
          (should= "" (str err)))))))