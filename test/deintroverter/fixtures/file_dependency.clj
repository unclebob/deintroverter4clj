(ns myapp.file-dependency-spec
  (:require [speclj.core :refer [describe it should]]
            [myapp.core :as core])
  (:import [java.io File]))

(describe "file dependency"
  (it "promotes filesystem assertion after sut side effect"
    (let [path "target/test-file-dependency.txt"]
      (core/write-text-file path "ok")
      (should (.exists (File. path)))))

  (it "stays introverted without a sut call"
    (should (.exists (File. "target/no-sut-call.txt")))))