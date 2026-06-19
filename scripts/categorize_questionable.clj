#!/usr/bin/env bb
(require '[babashka.classpath :refer [add-classpath]])
(add-classpath (str (System/getProperty "user.dir") "/scripts"))

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[deintroverter.analyze :as analyze]
         '[deintroverter.project :as project]
         '[deintroverter.sut :as sut]
         '[deintroverter.parse :as parse]
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

(defn- categorize [finding]
  (scan/categorize-finding project-root finding))

(defn- assertion-forms [finding]
  (mapcat (fn [t]
            (keep (fn [f] (when (seq? f) f))
                  [(or (:assertion-form t) (:asserted-form t))]))
          (get-in finding [:trace :assertions] [])))

(defn- unknown-macros [finding]
  (into #{}
        (keep (fn [form]
                (when-let [parsed (assertions/parse-assertion form)]
                  (when (= :unknown-assertion-macro (:reason parsed))
                    (first form))))
              (assertion-forms finding))))

(defn- collect-questionable []
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
                     :project-ctx ctx :add sut-ns :remove #{}})]
           (filter #(= :questionable (:verdict %))
                   (analyze/analyze-file path {:sut sut :project-ctx ctx})))
         (catch Throwable _ [])))
     spec-files)))

(let [findings (vec (collect-questionable))
      total (count findings)]
  (println "empire-2025 questionable:" total)
  (println)
  (println "By reason:")
  (pprint (frequencies (map :reason findings)))
  (println)
  (when (some #(= :unknown-assertion-macro (:reason %)) findings)
    (println "Unknown assertion macros (frequency):")
    (pprint (->> findings
                 (filter #(= :unknown-assertion-macro (:reason %)))
                 (mapcat unknown-macros)
                 frequencies
                 (sort-by val >)
                 (into (sorted-map-by (fn [a b] (compare (name b) (name a)))))))
    (println))
  (println "By category:")
  (->> (group-by categorize findings)
       (sort-by (comp - count val))
       (map (fn [[cat items]]
              {:category cat
               :count (count items)
               :files (count (group-by :file items))
               :reasons (frequencies (map :reason items))
               :examples (vec (take 3 (map #(select-keys % [:test-name :file :reason]) items)))}))
       pprint)
  (println)
  (println "Top files:")
  (->> (group-by :file findings)
       (sort-by (comp - count val))
       (take 15)
       (map (fn [[f v]]
              {:file (scan/rel-path project-root f)
               :count (count v)
               :reasons (frequencies (map :reason v))
               :category (name (categorize (first v)))}))
       pprint)
  (println)
  (println "All questionable tests:")
  (doseq [finding (sort-by (juxt :file :line) findings)]
    (let [{:keys [test-name file reason]} finding
          macros (when (= :unknown-assertion-macro reason)
                   (vec (unknown-macros finding)))]
      (println (str (scan/rel-path project-root file) "  " test-name "  " reason
                    (when (seq macros) (str "  " macros)))))))