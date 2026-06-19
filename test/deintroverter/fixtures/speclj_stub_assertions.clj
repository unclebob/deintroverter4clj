(ns myapp.stub-assertions-spec
  (:require [speclj.core :refer [describe it should-have-invoked should-not-have-invoked with-stubs stub with-redefs]]
            [myapp.core :as core]))

(describe "stub assertions"
  (with-stubs)
  (it "traces the preceding sut call for should-not-have-invoked"
    (with-redefs [core/calculate-total (stub :calculate-total)]
      (core/calculate-total [1 2])
      (should-not-have-invoked :calculate-total)))

  (it "traces the preceding sut call for should-have-invoked"
    (with-redefs [core/calculate-total (stub :calculate-total)]
      (core/calculate-total [1 2])
      (should-have-invoked :calculate-total {:with [[1 2]]}))))