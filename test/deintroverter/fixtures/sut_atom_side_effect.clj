(ns myapp.sut-atom-side-effect-spec
  (:require [speclj.core :refer [describe it should=]]
            [myapp.core :as core]))

(describe "sut atom side effect"
  (it "promotes direct reads of a sut-defined atom"
    (core/process 42)
    (should= 42 @core/process-state))

  (it "promotes derived reads of a sut-defined atom after a sut call"
    (core/process {:val 42})
    (should= 42 (get @core/process-state :val)))

  (it "stays introverted without a preceding sut call"
    (should= nil (get @core/process-state :val))))