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

(defn- analyze [file test-ns requires]
  (analyze/analyze-file (fixture file) {:sut (sut-for test-ns requires)}))

(deftest classifies-extroverted-deftest
  (is (= 1 (count (analyze "extroverted_direct.clj" 'myapp.core-test #{'myapp.core}))))
  (is (= :extroverted (:verdict (first (analyze "extroverted_direct.clj"
                                                'myapp.core-test #{'myapp.core}))))))

(deftest classifies-introverted-deftest
  (is (= :introverted (:verdict (first (analyze "introverted_literal.clj"
                                                'myapp.core-test #{}))))))

(deftest classifies-questionable-destructure
  (is (= :questionable (:verdict (first (analyze "questionable_destructure.clj"
                                                  'myapp.core-test #{'myapp.core}))))))

(deftest classifies-speclj-wrappers-as-extroverted
  (let [findings (analyze "speclj_wrappers.clj" 'myapp.wrapper-spec #{'myapp.core})]
    (is (= 3 (count findings)))
    (is (every? #(= :extroverted (:verdict %)) findings))))

(deftest classifies-through-setup-forms
  (let [findings (analyze "speclj_setup.clj" 'myapp.setup-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (every? #(= :extroverted (:verdict %)) findings))))