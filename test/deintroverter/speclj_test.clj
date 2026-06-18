(ns deintroverter.speclj-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [deintroverter.core :as core]))

(defn- extroverted-it? [finding]
  (and (= :it (:test-form finding))
       (= :extroverted (:verdict finding))))

(deftest analyzes-speclj-it-form
  (is (true?
       (some extroverted-it?
             (:findings
              (core/run!
               {:paths [(.getPath (io/file "test/deintroverter/fixtures/speclj_extroverted.clj"))]
                :project-root "test/deintroverter/fixtures/sample-project"
                :format :human :verbose true
                :add-sut #{}
                :remove-sut #{}}))))))