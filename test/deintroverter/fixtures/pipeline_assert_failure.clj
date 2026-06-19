(ns myapp.pipeline-assert-failure-spec
  (:require [speclj.core :refer [describe it should=]]))

(defn- assert-failure! [f]
  (should= 1 (f)))

(describe "assert-failure! helper"
  (it "walks through private defn- helper into assertions"
    (assert-failure! (constantly 1))))