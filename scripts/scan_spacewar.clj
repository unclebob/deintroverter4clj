#!/usr/bin/env bb
(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[deintroverter.core :as core]
         '[deintroverter.report :as report])

(def spacewar (or (System/getenv "SPACEWAR_ROOT")
                  (str (System/getProperty "user.dir") "/../spacewar")))

(defn ns-from-file [^java.io.File f]
  (try (some-> f slurp read-string second) (catch Throwable _ nil)))

(def spec-files
  (sort (map #(.getPath %)
             (filter #(and (.isFile %)
                           (.endsWith (.getName %) "_spec.clj"))
                     (file-seq (io/file spacewar "spec"))))))

(def sut-ns
  (into #{}
        (keep ns-from-file
              (filter #(.endsWith (.getName %) ".cljc")
                      (file-seq (io/file spacewar "src"))))))

(println "spacewar:" spacewar)
(println "scanning" (count spec-files) "files," (count sut-ns) "SUT namespaces")

(def aggregated
  (reduce
   (fn [acc path]
     (println " " (last (str/split path #"/")))
     (try
       (let [{:keys [findings errors]}
             (core/run! {:project-root spacewar
                         :format :human
                         :verbose false
                         :add-sut sut-ns
                         :remove-sut #{}
                         :help false
                         :paths [path]})]
         {:findings (into (:findings acc) findings)
          :errors (into (:errors acc) errors)})
       (catch Throwable _
         {:findings (:findings acc)
          :errors (conj (:errors acc)
                        {:type :scan-error
                         :file path
                         :message "StackOverflowError"})})))
   {:findings [] :errors []}
   spec-files))

(def findings (:findings aggregated))
(def errors (:errors aggregated))

(println)
(println "=== SUMMARY ===")
(pprint (:summary (report/build-edn spacewar findings errors)))

(println)
(println "=== NON-EXTROVERTED ===")
(doseq [{:keys [file line test-name verdict reason]}
        (sort-by (juxt :file :line) findings)
        :when (not (#{:extroverted :likely-extroverted} verdict))]
  (println (str (last (str/split file #"/")) ":" line
                "  " test-name "  " verdict
                (when reason (str "  (" reason ")")))))

(when (seq errors)
  (println)
  (println "=== ERRORS ===")
  (doseq [{:keys [file message]} errors]
    (println (last (str/split file #"/")) ":" message)))