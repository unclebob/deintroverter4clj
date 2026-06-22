(ns myapp.helper-destructure-result-spec
  (:require [speclj.core :refer [describe it should=]]
            [myapp.core :as core]))

(defn- run-helper [n]
  {:result (core/calculate-total [n])})

(describe "helper destructure result"
  (it "promotes assertions on a destructured helper result field"
    (let [{value :result} (run-helper 3)]
      (should= 3 value)))

  (it "stays introverted when helper result does not reach sut"
    (let [{value :result} {:result (str "literal")}]
      (should= "literal" value))))