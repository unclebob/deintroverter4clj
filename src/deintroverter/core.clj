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

Exit code is always 0; use the report for findings and errors.")

(defn print-usage []
  (println usage))

(defn- parse-flag [m args]
  [(assoc m :help true) (rest args)])

(defn- parse-kw-opt [k args]
  (fn [m _]
    [(assoc m k (keyword (second args))) (drop 2 args)]))

(defn- parse-str-opt [k args]
  (fn [m _]
    [(assoc m k (second args)) (drop 2 args)]))

(defn- parse-sym-set-opt [k args]
  (fn [m _]
    [(update m k conj (symbol (second args))) (drop 2 args)]))

(def ^:private base-options
  {:help false :format :human :verbose false :add-sut #{} :remove-sut #{}
   :project-root nil :paths []})

(defn- option-handlers [args]
  {"--help" parse-flag
   "-h" parse-flag
   "--format" (parse-kw-opt :format args)
   "--verbose" (fn [m _] [(assoc m :verbose true) (rest args)])
   "--project-root" (parse-str-opt :project-root args)
   "--sut-ns" (parse-sym-set-opt :add-sut args)
   "--exclude-ns" (parse-sym-set-opt :remove-sut args)})

(defn parse-args [args]
  (loop [m base-options args args]
    (if (empty? args)
      m
      (let [a (first args)]
        (if-let [handler (get (option-handlers args) a)]
          (let [[m' rest'] (handler m args)]
            (recur m' rest'))
          (if (.startsWith a "-")
            (throw (ex-info "Unknown option" {:opt a}))
            (recur (update m :paths conj a) (rest args))))))))

(defn- analyze-one-file [^java.io.File f ctx add-sut remove-sut]
  (let [content (slurp f)
        forms (parse/read-string-all content)
        ns-info (parse/parse-ns-form (first forms))
        sut (sut/infer-sut-namespaces
             {:test-namespace (:namespace ns-info)
              :requires (:requires ns-info)
              :project-ctx (or ctx {:in-project-namespaces #{}
                                    :external-dep-symbols #{}})
              :add add-sut
              :remove remove-sut})]
    (analyze/analyze-file (.getPath f) {:sut sut :project-ctx ctx})))

(defn- accumulate-file [state ^java.io.File f ctx add-sut remove-sut]
  (try
    (update state :findings into (analyze-one-file f ctx add-sut remove-sut))
    (catch Exception e
      (update state :errors conj {:type :parse-error
                                  :file (.getPath f)
                                  :message (.getMessage e)}))))

(defn- print-report [format root findings errors verbose]
  (if (= format :edn)
    (report/print-edn root findings errors)
    (report/print-human findings verbose)))

(defn run!
  [{:keys [help paths project-root format verbose add-sut remove-sut]}]
  (if help
    (do (print-usage) {:exit 0 :findings [] :errors []})
    (let [files (paths/collect-files paths)
          root (or project-root (some #(project/find-project-root (.getPath %)) files))
          ctx (when root (project/load-context root))
          {:keys [findings errors]}
          (reduce #(accumulate-file %1 %2 ctx add-sut remove-sut)
                  {:findings [] :errors []}
                  files)
          exit (report/exit-code findings errors)]
      (print-report format root findings errors verbose)
      {:exit exit :findings findings :errors errors})))

(defn -main [& args]
  (let [{:keys [exit]} (run! (parse-args args))]
    (System/exit exit)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-21T09:31:38.477583-05:00", :module-hash "-1984046852", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 8, :hash "1747452374"} {:id "def/usage", :kind "def", :line 10, :end-line 24, :hash "-1761203764"} {:id "defn/print-usage", :kind "defn", :line 26, :end-line 27, :hash "-2130808543"} {:id "defn-/parse-flag", :kind "defn-", :line 29, :end-line 30, :hash "-1736442577"} {:id "defn-/parse-kw-opt", :kind "defn-", :line 32, :end-line 34, :hash "1713272750"} {:id "defn-/parse-str-opt", :kind "defn-", :line 36, :end-line 38, :hash "-531651122"} {:id "defn-/parse-sym-set-opt", :kind "defn-", :line 40, :end-line 42, :hash "1922429449"} {:id "def/base-options", :kind "def", :line 44, :end-line 46, :hash "435610361"} {:id "defn-/option-handlers", :kind "defn-", :line 48, :end-line 55, :hash "-1333747564"} {:id "defn/parse-args", :kind "defn", :line 57, :end-line 67, :hash "1233204674"} {:id "defn-/analyze-one-file", :kind "defn-", :line 69, :end-line 80, :hash "1488701035"} {:id "defn-/accumulate-file", :kind "defn-", :line 82, :end-line 88, :hash "-1085413319"} {:id "defn-/print-report", :kind "defn-", :line 90, :end-line 93, :hash "-1641637040"} {:id "defn/run!", :kind "defn", :line 95, :end-line 108, :hash "-515700707"} {:id "defn/-main", :kind "defn", :line 110, :end-line 112, :hash "725954137"}]}
;; clj-mutate-manifest-end
