(ns myapp.speclj-with-fixture-spec
  (:require [speclj.core :refer [describe it should= with]]
            [myapp.core :as core]))

(declare initial-map literal-fixture)

(describe "speclj with fixture"
  (with initial-map (core/build-map 10 10))
  (with literal-fixture "plain")

  (it "promotes assertions on a fixture atom seeded by sut"
    (should= 10 (count (clojure.core/deref initial-map))))

  (it "stays introverted when the fixture value does not reach sut"
    (should= 5 (count literal-fixture))))