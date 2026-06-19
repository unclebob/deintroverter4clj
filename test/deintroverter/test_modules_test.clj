(ns deintroverter.test-modules-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.project :as project]
            [deintroverter.test-modules :as test-modules]))

(def project-ctx
  (project/load-context "test/deintroverter/fixtures/sample-project"))

(deftest detects-test-module-by-suffix
  (is (contains? (test-modules/infer-test-module-namespaces
                  {:test-namespace 'myapp.cloistered-spec
                   :requires #{'myapp.helpers-test 'myapp.core}
                   :sut #{'myapp.core}
                   :project-ctx project-ctx})
                 'myapp.helpers-test)))

(deftest accepts-distinct-test-module-namespace
  (is (test-modules/test-module-namespace?
        'myapp.helpers-test
        {:test-namespace 'myapp.cloistered-spec
         :sut #{'myapp.core}
         :project-ctx project-ctx})))

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

(deftest detects-test-module-from-alias-extra-paths
  (is (contains? (test-modules/infer-test-module-namespaces
                  {:test-namespace 'myapp.spec-mother-spec
                   :requires #{'myapp.spec-mother 'myapp.core}
                   :sut #{'myapp.core}
                   :project-ctx project-ctx})
                 'myapp.spec-mother)))