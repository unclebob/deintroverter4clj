(ns deintroverter.project-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [deintroverter.project :as project]))

(def fixture-root
  (.getPath (io/file "test/deintroverter/fixtures/sample-project")))

(deftest finds-deps-edn-walking-up
  (is (= fixture-root
         (project/find-project-root
          (.getPath (io/file fixture-root "src/myapp/core.clj")))))
  (is (= fixture-root (project/find-project-root fixture-root))
      "starts from project directory")
  (is (nil? (project/find-project-root "/tmp/no-deps-edn-here"))
      "returns nil when no deps.edn exists"))

(deftest discovers-in-project-namespaces
  (is (contains? (:in-project-namespaces (project/load-context fixture-root))
                 'myapp.core))
  (is (contains? (:external-dep-symbols (project/load-context fixture-root))
                 'org.clojure/test.check)))

(deftest discovers-namespaces-from-alias-extra-paths
  (let [ctx (project/load-context fixture-root)]
    (is (contains? (:in-project-namespaces ctx) 'myapp.spec-mother))
    (is (= "spec/myapp/spec_mother.clj"
           (get (:namespace-paths ctx) 'myapp.spec-mother)))))

(deftest discovers-cljc-namespace-with-reader-conditionals
  (let [ctx (project/load-context fixture-root)]
    (is (contains? (:in-project-namespaces ctx) 'myapp.reader-conditional))
    (is (= "src/myapp/reader_conditional.cljc"
           (get (:namespace-paths ctx) 'myapp.reader-conditional)))))