(ns myapp.callback-spy-wiring-spec
  (:require [speclj.core :refer [describe it should=]]
            [myapp.core :as core]))

(describe "callback spy wiring"
  (it "promotes when a callback injected into the sut writes a let-bound atom"
    (let [calls (atom [])]
      (core/run-with-handler (fn [v] (swap! calls conj v)) 42)
      (should= [42] @calls))))