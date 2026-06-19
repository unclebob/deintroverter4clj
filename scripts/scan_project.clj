#!/usr/bin/env bb
(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[deintroverter.analyze :as analyze]
         '[deintroverter.project :as project]
         '[deintroverter.report :as report]
         '[deintroverter.sut :as sut]
         '[deintroverter.parse :as parse])

(def project-root
  (or (System/getenv "PROJECT_ROOT")
      (throw (ex-info "Set PROJECT_ROOT" {}))))

(defn ns-from-file [^java.io.File f]
  (try (some-> f slurp read-string second) (catch Throwable _ nil)))

(defn- skip-dir? [^java.io.File dir]
  (#{".git" ".worktrees" "node_modules" "target"} (.getName dir)))

(defn- walk-files [^java.io.File dir]
  (when (.exists dir)
    (if (.isFile dir)
      [dir]
      (when-not (skip-dir? dir)
        (mapcat walk-files (.listFiles dir))))))

(defn spec-file? [^java.io.File f]
  (and (.isFile f)
       (let [n (.getName f)]
         (or (.endsWith n "_spec.clj")
             (.endsWith n "_test.clj")))))

(defn- deps-edn []
  (edn/read-string (slurp (io/file project-root "deps.edn"))))

(defn- scan-path-entries []
  (let [deps (deps-edn)
        extra (mapcat (fn [[_ a]] (or (:extra-paths a) [])) (:aliases deps))]
    (vec (distinct (concat (or (:paths deps) ["src"]) extra)))))

(defn- discover-spec-files []
  (vec
   (sort
    (for [entry (scan-path-entries)
          :let [dir (io/file project-root entry)]
          :when (.exists dir)
          f (walk-files dir)
          :when (spec-file? f)]
      (.getPath f)))))

(defn- discover-sut-namespaces []
  (into #{}
        (keep ns-from-file
              (filter #(let [n (.getName %)]
                         (or (.endsWith n ".clj")
                             (.endsWith n ".cljs")
                             (.endsWith n ".cljc")))
                      (file-seq (io/file project-root "src"))))))

(defn- analyze-file [path project-ctx sut-ns]
  (try
    (let [content (slurp path)
          forms   (parse/read-string-all content)
          ns-info (parse/parse-ns-form (first forms))
          sut     (sut/infer-sut-namespaces
                   {:test-namespace (:namespace ns-info)
                    :requires (:requires ns-info)
                    :project-ctx project-ctx
                    :add sut-ns
                    :remove #{}})]
      {:findings (analyze/analyze-file path {:sut sut :project-ctx project-ctx})
       :errors []})
    (catch Throwable e
      {:findings []
       :errors [{:type :scan-error :file path :message (or (.getMessage e) (str (class e)))}]})))

(let [spec-files   (discover-spec-files)
      sut-ns       (discover-sut-namespaces)
      project-ctx  (project/load-context project-root)]
  (println "project:" project-root)
  (println "paths:" (scan-path-entries))
  (println "scanning" (count spec-files) "spec files," (count sut-ns) "SUT namespaces")

  (def aggregated
    (reduce
     (fn [acc path]
       (print ".")
       (flush)
       (let [{:keys [findings errors]} (analyze-file path project-ctx sut-ns)]
         {:findings (into (:findings acc) findings)
          :errors (into (:errors acc) errors)}))
     {:findings [] :errors []}
     spec-files))

  (println)
  (def findings (:findings aggregated))
  (def errors (:errors aggregated))

  (println)
  (println "=== SUMMARY ===")
  (pprint (:summary (report/build-edn project-root findings errors)))

  (println)
  (println "=== NON-EXTROVERTED (first 50) ===")
  (doseq [{:keys [file line test-name verdict reason]}
          (take 50
                (filter (comp (complement #{:extroverted :likely-extroverted}) :verdict)
                        (sort-by (juxt :file :line) findings)))]
    (let [rel (str/replace file (str project-root "/") "")]
      (println (str rel ":" line "  " test-name "  " verdict
                    (when reason (str "  (" reason ")"))))))

  (let [non-ext (filter (comp (complement #{:extroverted :likely-extroverted}) :verdict) findings)]
    (when (> (count non-ext) 50)
      (println "... and" (- (count non-ext) 50) "more non-extroverted findings")))

  (when (seq errors)
    (println)
    (println "=== ERRORS (" (count errors) ") ===")
    (doseq [{:keys [file message]} (take 20 (sort-by :file errors))]
      (println (str/replace file (str project-root "/") "") ":" message))
    (when (> (count errors) 20)
      (println "... and" (- (count errors) 20) "more errors"))))