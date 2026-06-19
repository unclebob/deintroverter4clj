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
  (analyze/analyze-file (fixture file)
                        {:sut (sut-for test-ns requires)
                         :project-ctx project-ctx}))

(deftest classifies-extroverted-deftest
  (is (= 1 (count (analyze "extroverted_direct.clj" 'myapp.core-test #{'myapp.core}))))
  (is (= :extroverted (:verdict (first (analyze "extroverted_direct.clj"
                                                'myapp.core-test #{'myapp.core}))))))

(deftest classifies-introverted-deftest
  (is (= :introverted (:verdict (first (analyze "introverted_literal.clj"
                                                'myapp.core-test #{}))))))

(deftest classifies-cloistered-when-reaching-test-module
  (is (= :cloistered (:verdict (first (analyze "cloistered_helpers.clj"
                                                'myapp.cloistered-spec
                                                #{'myapp.core})))))
  (is (= :reaches-test-module (:reason (first (analyze "cloistered_helpers.clj"
                                                        'myapp.cloistered-spec
                                                        #{'myapp.core}))))))

(deftest classifies-cloistered-via-alias-extra-paths
  (is (= :cloistered (:verdict (first (analyze "cloistered_spec_mother.clj"
                                                'myapp.spec-mother-spec
                                                #{'myapp.core}))))))

(deftest classifies-extroverted-vector-destructure
  (is (= :extroverted (:verdict (first (analyze "questionable_destructure.clj"
                                                 'myapp.core-test #{'myapp.core}))))))

(deftest classifies-extroverted-map-keys-destructure
  (is (= :extroverted (:verdict (first (analyze "extroverted_destructure_keys.clj"
                                                 'myapp.destructure-keys-test
                                                 #{'myapp.core}))))))

(deftest classifies-extroverted-rest-destructure
  (is (= :extroverted (:verdict (first (analyze "extroverted_destructure_rest.clj"
                                                 'myapp.destructure-rest-test
                                                 #{'myapp.core}))))))

(deftest classifies-questionable-nested-destructure
  (is (= :questionable (:verdict (first (analyze "questionable_nested_destructure.clj"
                                                  'myapp.nested-destructure-test
                                                  #{'myapp.core}))))))

(deftest classifies-speclj-wrappers-as-extroverted
  (let [findings (analyze "speclj_wrappers.clj" 'myapp.wrapper-spec #{'myapp.core})]
    (is (= 3 (count findings)))
    (is (every? #(= :extroverted (:verdict %)) findings))))

(deftest classifies-through-setup-forms
  (let [findings (analyze "speclj_setup.clj" 'myapp.setup-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (every? #(= :extroverted (:verdict %)) findings))))

(deftest classifies-invoked-fn-literal-assertions
  (let [findings (analyze "speclj_fn_assertions.clj" 'myapp.fn-assertions-spec #{'myapp.core})]
    (is (= 1 (count findings)))
    (is (= :extroverted (:verdict (first findings))))))

(deftest classifies-should-greater-than-as-extroverted
  (is (= :extroverted (:verdict (first (analyze "speclj_should_gt.clj"
                                                 'myapp.should-gt-spec #{'myapp.core}))))))

(deftest classifies-stub-assertions-from-preceding-sut-call
  (let [findings (analyze "speclj_stub_assertions.clj" 'myapp.stub-assertions-spec #{'myapp.core})]
    (is (= 2 (count findings)))
    (is (every? #(= :extroverted (:verdict %)) findings))))

(deftest edn-findings-include-assertion-trace
  (let [{:keys [trace]} (first (analyze "extroverted_direct.clj"
                                        'myapp.core-test #{'myapp.core}))]
    (is (= 'myapp.core-test (:test-ns trace)))
    (is (contains? (:requires trace) 'myapp.core))
    (is (seq (:assertions trace)))
    (is (= :extroverted (:verdict (first (:assertions trace)))))))