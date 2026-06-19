(ns deintroverter.test-modules-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.project :as project]
            [deintroverter.sut :as sut]
            [deintroverter.test-modules :as test-modules]))

(def project-ctx
  (project/load-context "test/deintroverter/fixtures/sample-project"))

(defn- infer [test-ns requires sut]
  (test-modules/infer-test-module-namespaces
   {:test-namespace test-ns
    :requires requires
    :sut sut
    :project-ctx project-ctx}))

(deftest detects-test-module-by-suffix
  (is (contains? (infer 'myapp.cloistered-spec #{'myapp.helpers-test 'myapp.core}
                      #{'myapp.core})
                 'myapp.helpers-test)))

(deftest excludes-current-test-namespace
  (is (not (test-modules/test-module-namespace?
            'myapp.cloistered-spec
            {:test-namespace 'myapp.cloistered-spec
             :sut #{'myapp.core}
             :project-ctx project-ctx}))))

(deftest excludes-sut-namespaces
  (is (not (test-modules/test-module-namespace?
            'myapp.core
            {:test-namespace 'myapp.cloistered-spec
             :sut #{'myapp.core}
             :project-ctx project-ctx}))))