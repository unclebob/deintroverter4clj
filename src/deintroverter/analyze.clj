(ns deintroverter.analyze
  (:require [deintroverter.parse :as parse]
            [deintroverter.assertions :as assertions]
            [deintroverter.trace :as trace]))

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

(defn- destructuring-binding? [k]
  (not (symbol? k)))

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

(defn- process-forms [forms bindings trace-ctx]
  (mapcat
   (fn [form]
     (cond
       (not (seq? form))
       []

       (= 'let (first form))
       (let [after-bindings (rest form)
             binding-form   (first after-bindings)
             body           (rest after-bindings)
             pairs (if (vector? binding-form)
                       (partition 2 binding-form)
                       (partition 2 after-bindings))
             has-destructure? (some (fn [[k _]] (destructuring-binding? k)) pairs)
             new-bindings (reduce (fn [b [k v]]
                                    (if (symbol? k)
                                      (assoc b k v)
                                      b))
                                  bindings pairs)
             new-bindings (if has-destructure?
                            (assoc new-bindings :destructuring? true)
                            new-bindings)]
         (process-forms body new-bindings trace-ctx))

       (#{'do 'try 'catch 'finally} (first form))
       (process-forms (rest form) bindings trace-ctx)

       (#{'with-redefs 'doseq} (first form))
       (process-forms (drop 2 form) bindings trace-ctx)

       :else
       (let [parsed (assertions/parse-assertion form)]
         (if parsed
           (let [{:keys [asserted-form reason]} parsed]
             (if reason
               [{:verdict :questionable :reason reason}]
               [(trace/trace-form asserted-form bindings trace-ctx)]))
           (process-forms (rest form) bindings trace-ctx)))))
   forms))

(defn- test-verdict [results]
  (cond
    (some #(= :extroverted (:verdict %)) results)
    {:verdict :extroverted :reason nil}

    (some #(= :likely-extroverted (:verdict %)) results)
    {:verdict :likely-extroverted :reason :refer-all-heuristic}

    (some #(= :questionable (:verdict %)) results)
    {:verdict :questionable
     :reason (or (:reason (first (filter #(= :questionable (:verdict %)) results)))
                 :unknown)}

    (empty? results)
    {:verdict :introverted :reason :no-assertions}

    :else
    {:verdict :introverted :reason :no-sut-assertion}))

(defn analyze-file
  "Analyze a test file path. opts: {:sut #{namespace-syms}}
  Returns vector of finding maps."
  [file-path {:keys [sut]}]
  (let [content (slurp file-path)
        forms   (parse/read-string-all content)
        ns-form (first forms)
        ns-info (parse/parse-ns-form ns-form)
        trace-ctx (trace/make-trace-ctx ns-info sut (resolve-ns-fn ns-info))
        tests   (find-tests forms)]
    (vec
     (for [{:keys [form test-name body line]} tests
           :let [results (vec (process-forms body {} trace-ctx))
                 {:keys [verdict reason]} (test-verdict results)]]
       {:file file-path
        :line line
        :test-name (if (string? test-name) test-name (name test-name))
        :test-form form
        :verdict verdict
        :reason reason
        :sut-namespaces sut}))))