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

(defn- all-introverted []
  (let [spec-files (vec (sort (for [entry (scan-path-entries)
                                    :let [dir (io/file project-root entry)]
                                    :when (.exists dir)
                                    f (walk-files dir)
                                    :when (scan/spec-test-file? f)]
                                (.getPath f))))
        ctx        (project/load-context project-root)
        sut-ns     (scan/discover-sut-namespaces project-root ctx)]
    (mapcat (fn [path]
              (try
                (let [forms   (parse/read-string-all (slurp path))
                      ns-info (parse/parse-ns-form (first forms))
                      sut     (sut/infer-sut-namespaces
                               {:test-namespace (:namespace ns-info)
                                :requires (:requires ns-info)
                                :project-ctx ctx
                                :add sut-ns
                                :remove #{}})]
                  (filter #(= :introverted (:verdict %))
                          (analyze/analyze-file path {:sut sut :project-ctx ctx})))
                (catch Throwable _ [])))
            spec-files)))

(let [findings (vec (all-introverted))]
  (println "empire-2025 introverted:" (count findings))
  (println)
  (println "By reason:")
  (pprint (frequencies (map :reason findings)))
  (println)
  (println "By category:")
  (->> (group-by categorize findings)
       (sort-by (comp - count val))
       (map (fn [[cat items]]
              {:category cat
               :count (count items)
               :files (count (group-by :file items))
               :examples (vec (take 3 (map #(select-keys % [:test-name :file :reason]) items)))}))
       pprint)
  (println)
  (println "Top files:")
  (->> (group-by :file findings)
       (sort-by (comp - count val))
       (take 15)
       (map (fn [[f v]] {:file (scan/rel-path project-root f)
                         :count (count v)
                         :category (name (categorize (first v)))}))
       pprint))