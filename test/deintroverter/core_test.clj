(ns deintroverter.core-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [deintroverter.core :as core]))

(def sample-project "test/deintroverter/fixtures/sample-project")

(def introverted-fixture
  (.getPath (io/file "test/deintroverter/fixtures/introverted_literal.clj")))

(def extroverted-fixture
  (.getPath (io/file "test/deintroverter/fixtures/extroverted_direct.clj")))

(def run-opts-base
  {:project-root sample-project :add-sut #{} :remove-sut #{}})

(deftest cli-flags-introverted-fixture
  (is (= 0 (:exit (core/run! (assoc run-opts-base
                                    :paths [introverted-fixture]
                                    :format :human
                                    :verbose false)))))
  (is (pos? (count (:findings (core/run! (assoc run-opts-base
                                               :paths [introverted-fixture]
                                               :format :human
                                               :verbose false))))))
  (is (every? #{:introverted :questionable}
              (map :verdict (:findings (core/run! (assoc run-opts-base
                                                          :paths [introverted-fixture]
                                                          :format :human
                                                          :verbose false)))))))

(deftest help-prints-usage-and-exits-zero
  (let [out (with-out-str (core/run! {:help true :paths [] :format :human
                                      :verbose false :add-sut #{} :remove-sut #{}
                                      :project-root nil}))]
    (is (= 0 (:exit (core/run! {:help true :paths [] :format :human
                                :verbose false :add-sut #{} :remove-sut #{}
                                :project-root nil}))))
    (is (str/includes? out "deintroverter"))
    (is (str/includes? out "--format edn"))))

(deftest cli-edn-format
  (is (str/includes?
       (with-out-str (core/run! (assoc run-opts-base
                                       :paths [extroverted-fixture]
                                       :format :edn
                                       :verbose true)))
       ":findings"))
  (is (str/includes?
       (with-out-str (core/run! (assoc run-opts-base
                                       :paths [extroverted-fixture]
                                       :format :edn
                                       :verbose true)))
       ":extroverted")))