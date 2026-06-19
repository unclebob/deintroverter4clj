(ns deintroverter.test-modules
  (:require [clojure.set :as set]
            [deintroverter.sut :as sut]))

(defn test-layer-namespace?
  "True when ns-sym lives in the test layer (-spec/-test suffix or test/spec path)."
  [ns-sym project-ctx]
  (and (sut/eligible-for-analysis? ns-sym project-ctx)
       (or (sut/name-suffix-test-ns? ns-sym)
           (sut/under-test-path?
            (get-in project-ctx [:namespace-paths ns-sym])))))

(defn test-module-namespace?
  "True when ns-sym is another test-layer namespace per project conventions."
  [ns-sym {:keys [test-namespace sut project-ctx]}]
  (and (test-layer-namespace? ns-sym project-ctx)
       (not= ns-sym test-namespace)
       (not (contains? sut ns-sym))))

(defn infer-test-module-namespaces
  [{:keys [test-namespace requires sut project-ctx]}]
  (into #{}
        (filter #(test-module-namespace?
                 % {:test-namespace test-namespace
                    :sut sut
                    :project-ctx project-ctx})
                (set/union (:in-project-namespaces project-ctx)
                           (or requires #{})))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-19T12:53:13.755534-05:00", :module-hash "1442571816", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "-1763255898"} {:id "defn/test-layer-namespace?", :kind "defn", :line 5, :end-line 11, :hash "656428814"} {:id "defn/test-module-namespace?", :kind "defn", :line 13, :end-line 18, :hash "598565465"} {:id "defn/infer-test-module-namespaces", :kind "defn", :line 20, :end-line 28, :hash "-69383862"}]}
;; clj-mutate-manifest-end
