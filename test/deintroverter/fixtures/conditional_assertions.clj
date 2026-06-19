(ns myapp.conditional-spec
  (:require [speclj.core :refer [describe it should= should-not=]]
            [myapp.core :refer [answer]]))

(describe "conditionals"
  (it "asserts inside when"
    (let [x (answer)]
      (when x
        (should= 1 x))))

  (it "asserts inside if else branch"
    (let [x (answer)]
      (if false
        nil
        (should= 2 x))))

  (it "asserts inside dotimes"
    (dotimes [_ 3]
      (should= 1 (answer))))

  (it "asserts inside when-not"
    (when-not false
      (should-not= 0 (answer))))

  (it "asserts inside cond branch"
    (let [x (answer)]
      (cond
        false nil
        :else (should= 3 x)))))