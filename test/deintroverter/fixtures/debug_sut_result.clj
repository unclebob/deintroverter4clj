(ns myapp.debug-sut-result-spec
  (:require [speclj.core :refer [describe it should-contain]]
            [myapp.debug :as debug]))

(describe "debug sut result"
  (it "promotes assertions on a let-bound sut return value"
    (let [cell {:type :destroyer}
          result (debug/format-cell [0 0] cell)]
      (should-contain "destroyer" result)
      (should-contain ":" result)))

  (it "stays introverted when the let binding does not reach sut"
    (let [result (str "literal")]
      (should-contain "literal" result))))