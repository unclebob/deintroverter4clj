(ns deintroverter.core
  (:require [deintroverter.paths :as paths]
            [deintroverter.project :as project]
            [deintroverter.sut :as sut]
            [deintroverter.analyze :as analyze]
            [deintroverter.report :as report]
            [deintroverter.parse :as parse])
  (:gen-class))

(defn- parse-args [args]
  (loop [m    {:format :human :verbose false :add-sut #{} :remove-sut #{}
               :project-root nil :paths []}
         args args]
    (if (empty? args)
      m
      (let [a (first args)]
        (cond
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
  [{:keys [paths project-root format verbose add-sut remove-sut]}]
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
    {:exit exit :findings findings :errors errors}))

(defn -main [& args]
  (let [{:keys [exit]} (run! (parse-args args))]
    (System/exit exit)))