#!/usr/bin/env bb
(require '[babashka.classpath :refer [add-classpath]])
(add-classpath (str (System/getProperty "user.dir") "/scripts"))
(add-classpath (str (System/getProperty "user.dir") "/src"))

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[deintroverter.analyze :as analyze]
         '[deintroverter.project :as project]
         '[deintroverter.sut :as sut]
         '[deintroverter.parse :as parse]
         '[deintroverter.trace :as trace]
         '[deintroverter.test-modules :as test-modules]
         '[deintroverter.assertions :as assertions]
         '[scan-common :as scan])

(def project-root
  (or (System/getenv "PROJECT_ROOT")
      (throw (ex-info "Set PROJECT_ROOT" {}))))

(defn- deps-edn []
  (edn/read-string (slurp (io/file project-root "deps.edn"))))

(defn- scan-path-entries []
  (scan/scan-path-entries (deps-edn)))

(defn- skip-dir? [d]
  (scan/skip-directory? d))

(defn- walk-files [dir]
  (when (.exists dir)
    (if (.isFile dir) [dir]
        (when-not (skip-dir? dir) (mapcat walk-files (.listFiles dir))))))

(defn- resolve-ns-fn [{:keys [aliases]}]
  (fn [sym-or-alias]
    (when (symbol? sym-or-alias)
      (if-let [ns-part (namespace sym-or-alias)]
        (if-let [resolved (get aliases (symbol ns-part))]
          (name resolved)
          ns-part)
        (if-let [resolved (get aliases sym-or-alias)]
          (name resolved)
          (name sym-or-alias))))))

(defn- find-tests [forms]
  (mapcat
   (fn [form]
     (when (seq? form)
       (case (first form)
         deftest [{:form :deftest :test-name (second form) :body (drop 2 form)
                   :line (or (:line (meta form)) (:row (meta form)))}]
         it      [{:form :it :test-name (second form) :body (drop 2 form)
                   :line (or (:line (meta form)) (:row (meta form)))}]
         describe (find-tests (rest form))
         context  (find-tests (rest form))
         nil)))
   forms))

(defn- body-form [body]
  (if (= 1 (count body))
    (first body)
    (cons 'do body)))

(defn- assertion-form? [form]
  (boolean (or (assertions/parse-assertion form)
               (and (assertions/parse-assertion form) (:reason (assertions/parse-assertion form))))))

(defn- collect-forms [form]
  (cond
    (not (seq? form)) [form]
    (#{'let 'loop} (first form))
    (let [binding-form (second form)
          body (drop 2 form)]
      (into (mapcat collect-forms (if (vector? binding-form)
                                    (keep (fn [[k v]] (when (symbol? k) v))
                                          (partition 2 binding-form))
                                    (keep (fn [[k v]] (when (symbol? k) v))
                                          (partition 2 (rest form)))))
            (mapcat collect-forms body)))
    (#{'do 'try} (first form))
    (mapcat collect-forms (rest form))
    (#{'when 'when-not 'if} (first form))
    (mapcat collect-forms (rest form))
    :else [form]))

(defn- non-assertion-forms [body]
  (remove assertion-form? (collect-forms (body-form body))))

(defn- sut-pattern [body bindings trace-ctx]
  (cond
    (not (trace/reaches-sut-likely? (body-form body) bindings trace-ctx))
    :test-helper-only

    (some #(trace/reaches-sut-likely? % bindings trace-ctx)
          (non-assertion-forms body))
    :sut-action-side-effect

    :else
    :sut-only-in-unused-binding))

(defn- analyze-cloistered []
  (let [spec-files (vec (sort (for [entry (scan-path-entries)
                                    :let [dir (io/file project-root entry)]
                                    :when (.exists dir)
                                    f (walk-files dir)
                                    :when (scan/spec-test-file? f)]
                                (.getPath f))))
        ctx        (project/load-context project-root)
        sut-ns     (scan/discover-sut-namespaces project-root ctx)]
    (mapcat
     (fn [path]
       (try
         (let [forms     (parse/read-string-all (slurp path))
               ns-info   (parse/parse-ns-form (first forms))
               sut       (sut/infer-sut-namespaces
                          {:test-namespace (:namespace ns-info)
                           :requires (:requires ns-info)
                           :project-ctx ctx
                           :add sut-ns
                           :remove #{}})
               test-modules (test-modules/infer-test-module-namespaces
                             {:test-namespace (:namespace ns-info)
                              :requires (:requires ns-info)
                              :sut sut
                              :project-ctx ctx})
               trace-ctx (trace/make-trace-ctx
                          ns-info sut (resolve-ns-fn ns-info)
                          {:test-modules test-modules})
               tests     (find-tests forms)
               findings  (analyze/analyze-file path {:sut sut :project-ctx ctx})
               by-line   (group-by :line findings)]
           (for [{:keys [line test-name body]} tests
                 :let [finding (first (get by-line line))
                       verdict (:verdict finding)]
                 :when (= :cloistered verdict)]
             (assoc finding
                    :pattern (sut-pattern body {} trace-ctx)
                    :category (scan/categorize-finding project-root finding))))
         (catch Throwable _ [])))
     spec-files)))

(let [findings (vec (analyze-cloistered))
      total (count findings)
      by-pattern (frequencies (map :pattern findings))]
  (println "empire-2025 cloistered:" total)
  (println)
  (println "SUT involvement in test body:")
  (pprint by-pattern)
  (println)
  (println "Calls SUT but asserts via test helpers (side-effect tests):")
  (println " " (+ (get by-pattern :sut-action-side-effect 0)
                  (get by-pattern :sut-only-in-unused-binding 0))
         "of" total
         (format "(%.1f%%)"
                 (* 100.0 (/ (+ (get by-pattern :sut-action-side-effect 0)
                                (get by-pattern :sut-only-in-unused-binding 0))
                             total))))
  (println)
  (println "  :sut-action-side-effect — SUT call outside assertions, assert on test state")
  (println "  :sut-only-in-unused-binding — SUT result bound but assertion ignores it")
  (println "  :test-helper-only — no SUT call; reads setup/atoms via test utils only")
  (println)
  (println "Side-effect tests by category:")
  (->> (filter #(not= :test-helper-only (:pattern %)) findings)
       (group-by :category)
       (sort-by (comp - count val))
       (map (fn [[cat items]]
              {:category cat
               :count (count items)
               :action-side-effect (count (filter #(= :sut-action-side-effect (:pattern %)) items))
               :unused-binding (count (filter #(= :sut-only-in-unused-binding (:pattern %)) items))}))
       pprint)
  (println)
  (println "Test-helper-only by category:")
  (->> (filter #(= :test-helper-only (:pattern %)) findings)
       (group-by :category)
       (sort-by (comp - count val))
       (map (fn [[cat items]] {:category cat :count (count items)}))
       pprint)
  (println)
  (println "Examples — sut-action-side-effect:")
  (doseq [{:keys [test-name file pattern]} (take 5
                                                 (filter #(= :sut-action-side-effect (:pattern %))
                                                         findings))]
    (println " " (scan/rel-path project-root file) ":" test-name))
  (println)
  (println "Examples — test-helper-only:")
  (doseq [{:keys [test-name file pattern]} (take 5
                                                 (filter #(= :test-helper-only (:pattern %))
                                                         findings))]
    (println " " (scan/rel-path project-root file) ":" test-name)))