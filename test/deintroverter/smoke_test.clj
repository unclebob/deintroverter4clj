(ns deintroverter.smoke-test
  (:require [clojure.test :refer [deftest is testing]]))

(deftest project-loads
  (is (string? "deintroverter")))