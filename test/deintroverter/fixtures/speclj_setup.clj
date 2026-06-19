(ns myapp.setup-spec
  (:require [speclj.core :refer :all]
            [myapp.core :as core]))

(defn with-temp-value [f]
  (f 42))

(describe "setup forms"
  (it "walks through a local helper into assertions"
    (with-temp-value
      (fn [n]
        (should= 2 (core/calculate-total [n n])))))

  (it "walks past a bare sut call to sibling assertions"
    (with-redefs [core/calculate-total (constantly 99)]
      (core/calculate-total [1 2])
      (should= 99 (core/calculate-total [1 2])))))