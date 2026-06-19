(ns myapp.wrapper-spec
  (:require [speclj.core :refer :all]
            [myapp.core :as core]))

(describe "wrappers"
  (it "with-redefs passes through to assertions"
    (with-redefs [clojure.core/identity identity]
      (should= 2 (core/calculate-total [1 2]))))

  (it "doseq passes through to assertions"
    (doseq [n [1]]
      (should= n (first (core/calculate-total [n])))))

  (it "should-not-throw traces the guarded form"
    (should-not-throw (core/calculate-total [1]))))