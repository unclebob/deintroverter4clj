(ns myapp.cloistered-spec
  (:require [speclj.core :refer [describe it should=]]
            [myapp.helpers-test :as helpers]))

(describe "cloistered"
  (it "asserts via another test module without reaching SUT"
    (should= true (helpers/valid-input? 1))))