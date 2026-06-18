(ns deintroverter.analyze-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [deintroverter.analyze :as analyze]
            [deintroverter.project :as project]
            [deintroverter.sut :as sut]))

(defn- fixture [name]
  (.getPath (io/file "test/deintroverter/fixtures" name)))

(def project-ctx
  (project/load-context "test/deintroverter/fixtures/sample-project"))

(defn- sut-for [test-ns requires]
  (sut/infer-sut-namespaces
   {:test-namespace test-ns :requires requires
    :project-ctx project-ctx :add #{} :remove #{}}))

(deftest classifies-extroverted-deftest
  (let [findings (analyze/analyze-file (fixture "extroverted_direct.clj")
                                       {:sut (sut-for 'myapp.core-test #{'myapp.core})})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-introverted-deftest
  (let [findings (analyze/analyze-file (fixture "introverted_literal.clj")
                                       {:sut (sut-for 'myapp.core-test #{})})]
    (is (= :introverted (:verdict (first findings))))))

(deftest classifies-questionable-destructure
  (let [findings (analyze/analyze-file (fixture "questionable_destructure.clj")
                                       {:sut (sut-for 'myapp.core-test #{'myapp.core})})]
    (is (= :questionable (:verdict (first findings))))))