(ns myapp.cloistered-refer-all-spec
  (:require [speclj.core :refer [describe it should= should]]
            [myapp.helpers-test :refer :all]))

(describe "cloistered via refer :all"
  (it "promotes unqualified test-module calls in assertions"
    (should= true (valid-input? 1)))

  (it "promotes refer-all helper nested in assertion"
    (should (vector? (build-test-map ["x"])))))