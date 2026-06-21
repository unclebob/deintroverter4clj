(ns deintroverter.sut
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn- load-denylist []
  (-> "deintroverter/test_lib_denylist.edn"
      io/resource
      slurp
      edn/read-string))

(defn- clojure-suite? [ns-sym]
  (let [s (name ns-sym)]
    (or (= "clojure" (namespace ns-sym))
        (.startsWith s "clojure."))))

(defn- denied-test-lib? [ns-sym denylist]
  (or (contains? denylist ns-sym)
      (when-let [n (namespace ns-sym)]
        (contains? denylist (symbol n)))))

(defn name-suffix-test-ns?
  "True when the namespace name ends with -spec or -test."
  [ns-sym]
  (let [n (name ns-sym)]
    (or (.endsWith n "-spec")
        (.endsWith n "-test"))))

(defn under-test-path?
  "True when a project-relative path is under test/ or spec/."
  [rel-path]
  (boolean
   (when rel-path
     (or (re-find #"(^|/)test/" rel-path)
         (re-find #"(^|/)spec/" rel-path)))))

(defn- test-layer-namespace? [ns-sym project-ctx]
  (or (name-suffix-test-ns? ns-sym)
      (under-test-path? (get-in project-ctx [:namespace-paths ns-sym]))))

(defn- convention-candidate [test-ns]
  (let [n (name test-ns)]
    (cond
      (.endsWith n "-test")
      (symbol (namespace test-ns) (subs n 0 (- (count n) 5)))

      (.endsWith n "-spec")
      (symbol (namespace test-ns) (subs n 0 (- (count n) 5)))

      :else nil)))

(defn- eligible? [ns-sym {:keys [project-ctx denylist]}]
  (and (contains? (:in-project-namespaces project-ctx) ns-sym)
       (not (clojure-suite? ns-sym))
       (not (denied-test-lib? ns-sym denylist))
       (not (contains? (:external-dep-symbols project-ctx) ns-sym))))

(defn eligible-for-analysis?
  "True when ns-sym is an in-project namespace, excluding clojure, test libs, and deps."
  [ns-sym project-ctx]
  (eligible? ns-sym {:project-ctx project-ctx :denylist (load-denylist)}))

(defn infer-sut-namespaces
  [{:keys [test-namespace requires project-ctx add remove]}]
  (let [denylist (load-denylist)
        ctx      {:project-ctx project-ctx :denylist denylist}
        candidates (into #{}
                         (concat
                          (keep convention-candidate [test-namespace])
                          requires))]
    (-> (set (filter #(and (eligible? % ctx)
                           (not (test-layer-namespace? % project-ctx)))
                    candidates))
        (into add)
        (set/difference remove))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-21T09:31:57.669155-05:00", :module-hash "-1987572228", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 4, :hash "1815764547"} {:id "defn-/load-denylist", :kind "defn-", :line 6, :end-line 10, :hash "936438411"} {:id "defn-/clojure-suite?", :kind "defn-", :line 12, :end-line 15, :hash "-2058920951"} {:id "defn-/denied-test-lib?", :kind "defn-", :line 17, :end-line 20, :hash "475694154"} {:id "defn/name-suffix-test-ns?", :kind "defn", :line 22, :end-line 27, :hash "545162430"} {:id "defn/under-test-path?", :kind "defn", :line 29, :end-line 35, :hash "257161598"} {:id "defn-/test-layer-namespace?", :kind "defn-", :line 37, :end-line 39, :hash "918185605"} {:id "defn-/convention-candidate", :kind "defn-", :line 41, :end-line 50, :hash "-1073030516"} {:id "defn-/eligible?", :kind "defn-", :line 52, :end-line 56, :hash "-1653488020"} {:id "defn/eligible-for-analysis?", :kind "defn", :line 58, :end-line 61, :hash "-921959192"} {:id "defn/infer-sut-namespaces", :kind "defn", :line 63, :end-line 75, :hash "396679171"}]}
;; clj-mutate-manifest-end
