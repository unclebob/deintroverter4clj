#!/usr/bin/env bb
(require '[babashka.classpath :refer [add-classpath]])
(add-classpath (str (System/getProperty "user.dir") "/scripts"))
(add-classpath (str (System/getProperty "user.dir") "/src"))

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
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

(defn- sut-invoke? [form trace-ctx]
  (or (trace/direct-sut-invoke-form? form trace-ctx)
      (trace/reaches-sut-likely? form {} trace-ctx)))

(declare walk-sequential)

(defn- binding-from-test-module? [bindings trace-ctx]
  (boolean
   (some (fn [[sym origin]]
           (and (symbol? sym)
                (trace/reaches-test-module? origin bindings trace-ctx)))
         bindings)))

(defn- record-assertion [asserted bindings trace-ctx preceding seen-sut? counters]
  (let [intro? (= :introverted (:verdict (trace/trace-form asserted bindings trace-ctx)))
        test-mod? (trace/reaches-test-module? asserted bindings trace-ctx)
        test-state-binding? (binding-from-test-module? bindings trace-ctx)
        immediate? (and preceding (sut-invoke? preceding trace-ctx))]
    (cond-> counters
      (and intro? immediate?)
      (update :introverted-after-sut-assertions (fnil inc 0))

      (and intro? test-mod? immediate?)
      (update :test-module-assert-after-sut (fnil inc 0))

      (and intro? test-mod? seen-sut?)
      (update :test-module-assert-after-any-sut (fnil inc 0))

      (and intro? seen-sut? test-state-binding?)
      (update :introverted-after-sut-via-binding (fnil inc 0))

      (and intro? seen-sut?)
      (update :introverted-after-any-sut (fnil inc 0)))))

(defn- walk-sequential
  [forms bindings trace-ctx {:keys [preceding seen-sut?] :as state} counters]
  (loop [forms (seq forms)
         bindings bindings
         {:keys [preceding seen-sut?] :as state} state
         counters counters]
    (if (empty? forms)
      {:state state :counters counters}
      (let [form (first forms)
            parsed (when (seq? form) (assertions/parse-assertion form))]
        (cond
          (not (seq? form))
          (recur (rest forms) bindings state counters)

          (and parsed (not (:reason parsed)))
          (recur (rest forms) bindings state
                 (record-assertion (:asserted-form parsed) bindings trace-ctx
                                   preceding seen-sut? counters))

          (#{'let 'loop} (first form))
          (let [binding-form (second form)
                body (drop 2 form)
                new-bindings (if (vector? binding-form)
                               (reduce (fn [b [k v]]
                                         (if (symbol? k) (assoc b k v) b))
                                       bindings (partition 2 binding-form))
                               bindings)
                {:keys [state counters]}
                (walk-sequential body new-bindings trace-ctx state counters)]
            (recur (rest forms) bindings state counters))

          (#{'do 'try 'with-redefs} (first form))
          (let [inner (if (= 'with-redefs (first form)) (drop 2 form) (rest form))
                {:keys [state counters]}
                (walk-sequential inner bindings trace-ctx state counters)]
            (recur (rest forms) bindings state counters))

          (sut-invoke? form trace-ctx)
          (recur (rest forms) bindings
                 {:preceding form :seen-sut? true}
                 counters)

          :else
          (recur (rest forms) bindings
                 (assoc state :preceding form)
                 counters))))))

(defn- evidence-for-body [body trace-ctx]
  (:counters (walk-sequential body {} trace-ctx {:preceding nil :seen-sut? false}
                              {:introverted-after-sut-assertions 0
                               :test-module-assert-after-sut 0
                               :test-module-assert-after-any-sut 0
                               :introverted-after-sut-via-binding 0
                               :introverted-after-any-sut 0})))

(defn- collect-cloistered []
  (let [spec-files (vec (sort (for [entry (scan-path-entries)
                                    :let [dir (io/file project-root entry)]
                                    :when (.exists dir)
                                    f (walk-files dir)
                                    :when (scan/spec-test-file? f)]
                                (.getPath f))))
        ctx (project/load-context project-root)
        sut-ns (scan/discover-sut-namespaces project-root ctx)]
    (mapcat
     (fn [path]
       (try
         (let [forms (parse/read-string-all (slurp path))
               ns-info (parse/parse-ns-form (first forms))
               sut (sut/infer-sut-namespaces
                    {:test-namespace (:namespace ns-info)
                     :requires (:requires ns-info)
                     :project-ctx ctx :add sut-ns :remove #{}})
               trace-ctx (trace/make-trace-ctx
                          ns-info sut (resolve-ns-fn ns-info)
                          {:test-modules (test-modules/infer-test-module-namespaces
                                          {:test-namespace (:namespace ns-info)
                                           :requires (:requires ns-info)
                                           :sut sut :project-ctx ctx})})
               by-line (group-by :line (analyze/analyze-file path {:sut sut :project-ctx ctx}))]
           (for [{:keys [line test-name body]} (find-tests forms)
                 :let [finding (first (filter #(= test-name (:test-name %)) (get by-line line)))
                       evidence (evidence-for-body body trace-ctx)]
                 :when (= :cloistered (:verdict finding))]
             (assoc finding :evidence evidence)))
         (catch Throwable _ [])))
     spec-files)))

(let [findings (vec (collect-cloistered))
      total (count findings)
      strict #(pos? (get-in % [:evidence :test-module-assert-after-sut] 0))
      via-binding #(pos? (get-in % [:evidence :introverted-after-sut-via-binding] 0))
      any-intro-sut #(pos? (get-in % [:evidence :introverted-after-any-sut] 0))
      promotable #(or (strict %) (via-binding %))]
  (println "empire-2025 cloistered:" total)
  (println)
  (println "Promotion evidence tiers:")
  (println)
  (println "  T1 — assertion immediately after SUT, reads test-module in assert form:")
  (println "   " (count (filter strict findings)) "tests"
          (format "(%.1f%%)" (* 100.0 (/ (count (filter strict findings)) total))))
  (println)
  (println "  T2 — prior SUT + introverted assert via let-bound test-state read:")
  (println "   " (count (filter via-binding findings)) "tests with binding evidence")
  (println)
  (println "  T1+T2 combined (promotable to likely-extroverted):")
  (println "   " (count (filter promotable findings)) "tests"
          (format "(%.1f%%)" (* 100.0 (/ (count (filter promotable findings)) total))))
  (println)
  (println "  T3 — any prior SUT + any introverted assertion (weaker):")
  (println "   " (count (filter any-intro-sut findings)) "tests"
          (format "(%.1f%%)" (* 100.0 (/ (count (filter any-intro-sut findings)) total))))
  (println)
  (println "  Remaining cloistered (no SUT/action link to assertion):")
  (println "   " (count (remove promotable findings)) "tests"))