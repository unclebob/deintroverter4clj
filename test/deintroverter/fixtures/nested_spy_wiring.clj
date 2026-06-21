(ns myapp.nested-spy-wiring-spec
  (:require [speclj.core :refer [describe it should]]
            [myapp.core :as core]))

(describe "nested spy wiring"
  (it "promotes nested deref reads of a wired spy atom"
    (let [call-count (atom 0)]
      (with-redefs [core/process (fn [x] (swap! call-count inc) x)]
        (core/process 1)
        (should (<= 1 @call-count))))))