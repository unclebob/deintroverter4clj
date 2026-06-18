(ns deintroverter.sut-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.sut :as sut]))

(def project-ctx
  {:in-project-namespaces #{'myapp.core 'myapp.helpers}
   :external-dep-symbols #{'org.clojure/test.check}})

(deftest convention-strips-test-suffix
  (is (= #{'myapp.core}
         (sut/infer-sut-namespaces
          {:test-namespace 'myapp.core-test
           :requires #{}
           :project-ctx project-ctx
           :add #{} :remove #{}}))))

(deftest excludes-clojure-and-test-libs
  (let [sut-ns (sut/infer-sut-namespaces
                {:test-namespace 'myapp.core-test
                 :requires #{'clojure.test 'speclj.core 'myapp.core}
                 :project-ctx project-ctx
                 :add #{} :remove #{}})]
    (is (contains? sut-ns 'myapp.core))
    (is (not (contains? sut-ns 'clojure.test)))
    (is (not (contains? sut-ns 'speclj.core)))))

(deftest cli-add-and-remove-overrides
  (is (= #{'myapp.extra}
         (sut/infer-sut-namespaces
          {:test-namespace 'myapp.core-test
           :requires #{'myapp.core}
           :project-ctx project-ctx
           :add #{'myapp.extra}
           :remove #{'myapp.core}}))))