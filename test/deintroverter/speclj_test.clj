(ns deintroverter.speclj-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [deintroverter.core :as core]))

(deftest analyzes-speclj-it-form
  (let [fixture (.getPath (io/file "test/deintroverter/fixtures/speclj_extroverted.clj"))
        {:keys [findings]} (core/run!
                            {:paths [fixture]
                             :project-root "test/deintroverter/fixtures/sample-project"
                             :format :human :verbose true
                             :add-sut #{} :remove-sut #{}})]
    (is (some #(and (= :it (:test-form %)) (= :extroverted (:verdict %))) findings))))