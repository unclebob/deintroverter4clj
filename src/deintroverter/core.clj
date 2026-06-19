(ns deintroverter.core
  (:require [deintroverter.paths :as paths]
            [deintroverter.project :as project]
            [deintroverter.sut :as sut]
            [deintroverter.analyze :as analyze]
            [deintroverter.report :as report]
            [deintroverter.parse :as parse])
  (:gen-class))

(def usage
  "deintroverter — classify Clojure/Speclj tests by SUT grounding

Usage:
  bb -m deintroverter.core [options] <paths...>

Options:
  -h, --help              Print this help and exit
  --format edn            Structured EDN output (default: human)
  --verbose               Show extroverted and likely-extroverted tests
  --project-root <path>   Project root for deps.edn discovery
  --sut-ns <namespace>    Add a namespace to the SUT set
  --exclude-ns <namespace>  Remove a namespace from the SUT set

Exit code 0 when no introverted, questionable, or parse errors; 1 otherwise.")

(defn print-usage []
  (println usage))

(defn- parse-args [args]
  (loop [m    {:help false :format :human :verbose false :add-sut #{} :remove-sut #{}
               :project-root nil :paths []}
         args args]
    (if (empty? args)
      m
      (let [a (first args)]
        (cond
          (#{"--help" "-h"} a)   (recur (assoc m :help true) (rest args))

          (= "--format" a)      (recur (assoc m :format (keyword (second args)))
                                      (drop 2 args))
          (= "--verbose" a)      (recur (assoc m :verbose true) (rest args))
          (= "--project-root" a) (recur (assoc m :project-root (second args))
                                         (drop 2 args))
          (= "--sut-ns" a)       (recur (update m :add-sut conj (symbol (second args)))
                                         (drop 2 args))
          (= "--exclude-ns" a)   (recur (update m :remove-sut conj (symbol (second args)))
                                         (drop 2 args))
          (.startsWith a "-")    (throw (ex-info "Unknown option" {:opt a}))
          :else                  (recur (update m :paths conj a) (rest args)))))))

(defn run!
  [{:keys [help paths project-root format verbose add-sut remove-sut]}]
  (if help
    (do (print-usage) {:exit 0 :findings [] :errors []})
    (let [files (paths/collect-files paths)
        root  (or project-root
                  (some #(project/find-project-root (.getPath %)) files))
        ctx   (when root (project/load-context root))
        {:keys [findings errors]}
        (reduce
         (fn [{:keys [findings errors]} ^java.io.File f]
           (try
             (let [content (slurp f)
                   forms   (parse/read-string-all content)
                   ns-form (first forms)
                   ns-info (parse/parse-ns-form ns-form)
                   sut     (sut/infer-sut-namespaces
                            {:test-namespace (:namespace ns-info)
                             :requires       (:requires ns-info)
                             :project-ctx    (or ctx {:in-project-namespaces #{}
                                                      :external-dep-symbols #{}})
                             :add            add-sut
                             :remove         remove-sut})
                   file-findings (analyze/analyze-file (.getPath f) {:sut sut})]
               {:findings (into findings file-findings)
                :errors errors})
             (catch Exception e
               {:findings findings
                :errors (conj errors {:type :parse-error
                                      :file (.getPath f)
                                      :message (.getMessage e)})})))
         {:findings [] :errors []}
         files)
        exit (report/exit-code findings errors)]
      (case format
        :edn (report/print-edn root findings errors)
        (report/print-human findings verbose))
      {:exit exit :findings findings :errors errors})))

(defn -main [& args]
  (let [{:keys [exit]} (run! (parse-args args))]
    (System/exit exit)))