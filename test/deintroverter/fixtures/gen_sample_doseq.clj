(ns myapp.gen-sample-doseq-spec
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [myapp.core :refer [round]]
            [speclj.core :refer [describe it should]]))

(s/def ::number number?)

(describe "generative doseq"
  (it "allows unguarded gen/sample over sut"
    (doseq [x (gen/sample (s/gen ::number))]
      (should (int? (round x)))))

  (it "stays conditional when gen/sample size is zero"
    (doseq [x (gen/sample (s/gen ::number) 0)]
      (should (int? (round x))))))