(ns deintroverter.smoke-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.report :as report]))

(deftest clean-run-exits-zero
  (is (= 0 (report/exit-code [] []))))