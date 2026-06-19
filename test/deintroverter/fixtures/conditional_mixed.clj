(ns myapp.conditional-mixed-spec
  (:require [speclj.core :refer [describe it should=]]
            [myapp.core :refer [answer]]))

(describe "mixed"
  (it "unconditional assertion outranks conditional"
    (should= 1 (answer))
    (when false
      (should= 2 (answer)))))