(ns deintroverter.sut-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.sut :as sut]))

(def project-ctx
  {:in-project-namespaces #{'myapp.core 'myapp.helpers 'myapp.helpers-test}
   :namespace-paths {'myapp.helpers-test "test/myapp/helpers_test.clj"}
   :external-dep-symbols #{'org.clojure/test.check}})

(def infer-opts
  {:project-ctx project-ctx :add #{} :remove #{}})

(deftest convention-strips-test-suffix
  (is (= #{'myapp.core}
         (sut/infer-sut-namespaces
          (assoc infer-opts :test-namespace 'myapp.core-test :requires #{})))))

(deftest convention-strips-spec-suffix
  (is (= #{'myapp.core}
         (sut/infer-sut-namespaces
          (assoc infer-opts :test-namespace 'myapp.core-spec :requires #{})))))

(deftest convention-strips-test-suffix-to-in-project-ns
  (is (= #{'myapp.helpers}
         (sut/infer-sut-namespaces
          (assoc infer-opts :test-namespace 'myapp.helpers-test :requires #{})))))

(deftest excludes-clojure-and-test-libs
  (let [opts (assoc infer-opts
                    :test-namespace 'myapp.core-test
                    :requires #{'clojure.test 'speclj.core 'myapp.core})]
    (is (contains? (sut/infer-sut-namespaces opts) 'myapp.core))
    (is (not (contains? (sut/infer-sut-namespaces opts) 'clojure.test)))
    (is (not (contains? (sut/infer-sut-namespaces opts) 'speclj.core)))))

(deftest excludes-test-layer-namespaces-from-sut
  (let [opts (assoc infer-opts
                    :test-namespace 'myapp.cloistered-spec
                    :requires #{'myapp.core 'myapp.helpers-test})]
    (is (contains? (sut/infer-sut-namespaces opts) 'myapp.core))
    (is (not (contains? (sut/infer-sut-namespaces opts) 'myapp.helpers-test)))))

(deftest cli-add-and-remove-overrides
  (is (= #{'myapp.extra}
         (sut/infer-sut-namespaces
          (assoc infer-opts
                 :test-namespace 'myapp.core-test
                 :requires #{'myapp.core}
                 :add #{'myapp.extra}
                 :remove #{'myapp.core})))))