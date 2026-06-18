(ns deintroverter.core-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [deintroverter.core :as core]))

(deftest cli-flags-introverted-fixture
  (let [fixture (.getPath (io/file "test/deintroverter/fixtures/introverted_literal.clj"))
        {:keys [exit findings]} (core/run!
                                 {:paths [fixture]
                                  :project-root "test/deintroverter/fixtures/sample-project"
                                  :format :human
                                  :verbose false
                                  :add-sut #{}
                                  :remove-sut #{}})]
    (is (= 1 exit))
    (is (pos? (count findings)))
    (is (every? #{:introverted :questionable} (set (map :verdict findings))))))

(deftest cli-edn-format
  (let [fixture (.getPath (io/file "test/deintroverter/fixtures/extroverted_direct.clj"))
        out     (with-out-str
                  (core/run!
                   {:paths [fixture]
                    :project-root "test/deintroverter/fixtures/sample-project"
                    :format :edn :verbose true
                    :add-sut #{} :remove-sut #{}}))]
    (is (re-find #":findings" out))
    (is (re-find #":extroverted" out))))