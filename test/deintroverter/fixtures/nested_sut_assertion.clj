(ns myapp.nested-sut-assertion-spec
  (:require [speclj.core :refer [describe it should]]
            [myapp.debug :as debug]))

(describe "nested sut in assertion"
  (it "promotes a production invoke nested inside re-matches"
    (should (re-matches #"pos" (debug/format-cell "pos" "cell"))))

  (it "stays introverted when no production invoke appears in the tree"
    (should (re-matches #"ok" (str "ok")))))