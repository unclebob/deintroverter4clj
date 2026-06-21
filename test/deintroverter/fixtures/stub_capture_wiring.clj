(ns myapp.stub-capture-wiring-spec
  (:require [speclj.core :refer [describe it should=]]
            [myapp.core :as core]))

(describe "stub capture wiring"
  (it "promotes when a redef stub resets a let-bound atom after sut call"
    (let [captured (atom nil)]
      (with-redefs [core/process (fn [x] (reset! captured x))]
        (core/process 42)
        (should= 42 @captured))))

  (it "stays introverted when the stub does not write the asserted atom"
    (let [captured (atom nil)]
      (with-redefs [core/process (fn [_] :ok)]
        (core/process 42)
        (should= 42 @captured)))))