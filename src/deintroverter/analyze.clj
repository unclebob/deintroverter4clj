(ns deintroverter.analyze
  (:require [deintroverter.parse :as parse]
            [deintroverter.project :as project]
            [deintroverter.test-modules :as test-modules]
            [deintroverter.trace :as trace]
            [deintroverter.walk :as walk]))

(defn- resolve-namespaced [aliases sym]
  (let [ns-part (namespace sym)]
    (if-let [resolved (get aliases (symbol ns-part))]
      (name resolved)
      ns-part)))

(defn- resolve-unqualified [aliases sym]
  (if-let [resolved (get aliases sym)]
    (name resolved)
    (name sym)))

(defn- resolve-ns-fn [{:keys [aliases]}]
  (fn [sym-or-alias]
    (when (symbol? sym-or-alias)
      (if (namespace sym-or-alias)
        (resolve-namespaced aliases sym-or-alias)
        (resolve-unqualified aliases sym-or-alias)))))

(defn- fn-form? [form]
  (and (seq? form) (#{'fn 'fn*} (first form))))

(defn- defn-arity-fn-literal [form]
  (list 'fn (nth form 2) (nth form 3)))

(defn- defn-docstring-fn-literal [form]
  (list 'fn (nth form 3) (nth form 4)))

(defn- defn-docstring? [form third]
  (and (string? third) (>= (count form) 5) (vector? (nth form 3))))

(defn- defn->fn-literal [form]
  (when (and (seq? form) (#{'defn 'defn-} (first form)) (>= (count form) 4))
    (let [third (nth form 2)]
      (if (vector? third)
        (defn-arity-fn-literal form)
        (when (defn-docstring? form third)
          (defn-docstring-fn-literal form))))))

(defn- fn-bindings-from-forms [forms]
  (into {}
        (keep (fn [form]
                (when-let [fn-lit (defn->fn-literal form)]
                  [(second form) fn-lit]))
              forms)))

(defn- fixture-bindings-from-forms [forms]
  (into {}
        (keep (fn [form]
                (when (and (seq? form) (= 'with (first form)) (>= (count form) 3)
                           (symbol? (second form)))
                  [(second form) (nth form 2)]))
              forms)))

(defn- test-line [form]
  (or (:line (meta form)) (:row (meta form))))

(defn- test-entry [form kind helper-bindings]
  {:form kind
   :test-name (second form)
   :body (drop 2 form)
   :line (test-line form)
   :helper-bindings helper-bindings})

(declare find-tests-in-forms)

(defn- find-tests-describe-step [form pending results inherited-helpers]
  (let [body (rest form)
        block-helpers (merge inherited-helpers
                             (fn-bindings-from-forms body)
                             (fixture-bindings-from-forms body))]
    {:pending (rest pending)
     :results (into results (find-tests-in-forms body block-helpers))}))

(defn- find-tests-form-step [form pending results inherited-helpers]
  (cond
    (not (seq? form))
    {:pending (rest pending) :results results}

    (#{'deftest 'it} (first form))
    {:pending (rest pending)
     :results (conj results (test-entry form
                                        (if (= 'deftest (first form)) :deftest :it)
                                        inherited-helpers))}

    (#{'describe 'context} (first form))
    (find-tests-describe-step form pending results inherited-helpers)

    :else
    {:pending (rest pending) :results results}))

(defn- find-tests-in-forms [forms inherited-helpers]
  (loop [pending (seq forms) results []]
    (if (empty? pending)
      results
      (let [form (first pending)
            {:keys [pending results]} (find-tests-form-step form pending results inherited-helpers)]
        (recur pending results)))))

(defn- find-tests [forms]
  (find-tests-in-forms (rest forms) (fn-bindings-from-forms forms)))

(defn- build-finding-trace [trace-ctx sut assertion-results]
  {:test-ns (:test-ns trace-ctx)
   :requires (:requires trace-ctx)
   :refer-syms (:all-refer-syms trace-ctx)
   :sut-namespaces sut
   :assertions (vec (map :trace assertion-results))})

(defn- body-form [body]
  (if (= 1 (count body))
    (first body)
    (cons 'do body)))

(defn- promote-cloistered [verdict body bindings trace-ctx]
  (if (and (= :introverted (:verdict verdict))
           (trace/reaches-test-module? (body-form body) bindings trace-ctx))
    {:verdict :cloistered :reason :reaches-test-module}
    verdict))

(defn- unconditional-assertion-results [results]
  (remove #(= :conditional-assertion (:verdict %)) results))

(defn- strongest-conditional-reason [results]
  (let [reasons (map :reason (filter #(= :conditional-assertion (:verdict %)) results))]
    (or (some #{:would-be-extroverted} reasons)
        (some #{:would-be-likely-extroverted} reasons)
        (some #{:unknown-assertion-macro :destructuring} reasons)
        (first reasons)
        :conditional-assertion)))

(defn- finding-conditional-cause [results]
  (let [causes (set (keep #(get-in % [:trace :conditional-cause])
                          (filter #(= :conditional-assertion (:verdict %)) results)))]
    (when (= 1 (count causes))
      (first causes))))

(defn- finding-conditional-context [results cause]
  (let [contexts (set (keep (fn [r]
                              (when (= cause (get-in r [:trace :conditional-cause]))
                                (get-in r [:trace :conditional-context])))
                            (filter #(= :conditional-assertion (:verdict %)) results)))]
    (when (= 1 (count contexts))
      (first contexts))))

(defn- first-unconditional-verdict [unconditional verdict]
  (first (filter #(= verdict (:verdict %)) unconditional)))

(defn- extroverted-verdict [unconditional]
  (when (some #(= :extroverted (:verdict %)) unconditional)
    {:verdict :extroverted :reason nil}))

(defn- likely-extroverted-verdict [unconditional]
  (when-let [match (first-unconditional-verdict unconditional :likely-extroverted)]
    (cond-> {:verdict :likely-extroverted
             :reason (or (:reason match) :refer-all-heuristic)}
      (:reason-legacy match) (assoc :reason-legacy (:reason-legacy match)))))

(defn- questionable-verdict [unconditional]
  (when-let [match (first-unconditional-verdict unconditional :questionable)]
    {:verdict :questionable :reason (or (:reason match) :unknown)}))

(defn- introverted-verdict [unconditional]
  (when (seq unconditional)
    {:verdict :introverted :reason :no-sut-assertion}))

(defn- unconditional-verdict [unconditional]
  (or (extroverted-verdict unconditional)
      (likely-extroverted-verdict unconditional)
      (questionable-verdict unconditional)
      (introverted-verdict unconditional)))

(defn- test-verdict [results]
  (or (unconditional-verdict (unconditional-assertion-results results))
      (when (some #(= :conditional-assertion (:verdict %)) results)
        {:verdict :conditional-assertion
         :reason (strongest-conditional-reason results)})
      {:verdict :introverted :reason :no-assertions}))

(defn- findings-for-forms [file-path forms {:keys [sut project-ctx]}]
  (let [ns-form (first forms)
        ns-info (parse/parse-ns-form ns-form)
        project-ctx (or project-ctx {:in-project-namespaces #{}
                                     :namespace-paths {}
                                     :external-dep-symbols #{}})
        test-modules (test-modules/infer-test-module-namespaces
                      {:test-namespace (:namespace ns-info)
                       :requires (:requires ns-info)
                       :sut sut
                       :project-ctx project-ctx})
        trace-ctx (trace/make-trace-ctx ns-info sut (resolve-ns-fn ns-info)
                                        {:test-modules test-modules})
        tests (find-tests forms)]
    (vec
     (for [{:keys [form test-name body line helper-bindings]} tests
           :let [results (vec (:results (walk/process-forms body helper-bindings trace-ctx)))
                 {:keys [verdict reason reason-legacy]}
                 (promote-cloistered (test-verdict results) body {} trace-ctx)
                 conditional-cause (finding-conditional-cause results)
                 conditional-context (finding-conditional-context results conditional-cause)]]
       (cond-> {:file file-path
                :line line
                :test-name (if (string? test-name) test-name (name test-name))
                :test-form form
                :verdict verdict
                :reason reason
                :sut-namespaces sut
                :trace (assoc (build-finding-trace trace-ctx sut results)
                              :test-modules test-modules)}
         reason-legacy (assoc :reason-legacy reason-legacy)
         conditional-cause (assoc :conditional-cause conditional-cause)
         conditional-context (assoc :conditional-context conditional-context))))))

(defn analyze-forms
  "Analyze parsed namespace forms. opts: {:sut #{namespace-syms} :project-ctx map}"
  [forms opts]
  (findings-for-forms "<forms>" forms opts))

(defn analyze-file
  "Analyze a test file path. opts: {:sut #{namespace-syms} :project-ctx map}
  Returns vector of finding maps."
  [file-path opts]
  (findings-for-forms file-path (parse/read-string-all (slurp file-path)) opts))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-22T09:17:53.725491-05:00", :module-hash "1923756716", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 6, :hash "1970830046"} {:id "defn-/resolve-namespaced", :kind "defn-", :line 8, :end-line 12, :hash "1409516810"} {:id "defn-/resolve-unqualified", :kind "defn-", :line 14, :end-line 17, :hash "718817384"} {:id "defn-/resolve-ns-fn", :kind "defn-", :line 19, :end-line 24, :hash "-746974098"} {:id "defn-/fn-form?", :kind "defn-", :line 26, :end-line 27, :hash "1534401844"} {:id "defn-/defn-arity-fn-literal", :kind "defn-", :line 29, :end-line 30, :hash "7584441"} {:id "defn-/defn-docstring-fn-literal", :kind "defn-", :line 32, :end-line 33, :hash "-396941995"} {:id "defn-/defn-docstring?", :kind "defn-", :line 35, :end-line 36, :hash "1220406710"} {:id "defn-/defn->fn-literal", :kind "defn-", :line 38, :end-line 44, :hash "-397160554"} {:id "defn-/fn-bindings-from-forms", :kind "defn-", :line 46, :end-line 51, :hash "-1555522518"} {:id "defn-/fixture-bindings-from-forms", :kind "defn-", :line 53, :end-line 59, :hash "1279766009"} {:id "defn-/test-line", :kind "defn-", :line 61, :end-line 62, :hash "-1424048643"} {:id "defn-/test-entry", :kind "defn-", :line 64, :end-line 69, :hash "-1355136209"} {:id "form/13/declare", :kind "declare", :line 71, :end-line 71, :hash "-224247137"} {:id "defn-/find-tests-describe-step", :kind "defn-", :line 73, :end-line 79, :hash "-1738422458"} {:id "defn-/find-tests-form-step", :kind "defn-", :line 81, :end-line 96, :hash "1363170017"} {:id "defn-/find-tests-in-forms", :kind "defn-", :line 98, :end-line 104, :hash "-1054650713"} {:id "defn-/find-tests", :kind "defn-", :line 106, :end-line 107, :hash "-571188571"} {:id "defn-/build-finding-trace", :kind "defn-", :line 109, :end-line 114, :hash "377525482"} {:id "defn-/body-form", :kind "defn-", :line 116, :end-line 119, :hash "576180413"} {:id "defn-/promote-cloistered", :kind "defn-", :line 121, :end-line 125, :hash "1925045456"} {:id "defn-/unconditional-assertion-results", :kind "defn-", :line 127, :end-line 128, :hash "-1754927494"} {:id "defn-/strongest-conditional-reason", :kind "defn-", :line 130, :end-line 136, :hash "-1142003073"} {:id "defn-/finding-conditional-cause", :kind "defn-", :line 138, :end-line 142, :hash "-890377974"} {:id "defn-/finding-conditional-context", :kind "defn-", :line 144, :end-line 150, :hash "554572935"} {:id "defn-/first-unconditional-verdict", :kind "defn-", :line 152, :end-line 153, :hash "1239701132"} {:id "defn-/extroverted-verdict", :kind "defn-", :line 155, :end-line 157, :hash "113686393"} {:id "defn-/likely-extroverted-verdict", :kind "defn-", :line 159, :end-line 163, :hash "-1503214070"} {:id "defn-/questionable-verdict", :kind "defn-", :line 165, :end-line 167, :hash "566982830"} {:id "defn-/introverted-verdict", :kind "defn-", :line 169, :end-line 171, :hash "1774438313"} {:id "defn-/unconditional-verdict", :kind "defn-", :line 173, :end-line 177, :hash "1145335451"} {:id "defn-/test-verdict", :kind "defn-", :line 179, :end-line 184, :hash "-2029536367"} {:id "defn-/findings-for-forms", :kind "defn-", :line 186, :end-line 218, :hash "1522342754"} {:id "defn/analyze-forms", :kind "defn", :line 220, :end-line 223, :hash "-169972165"} {:id "defn/analyze-file", :kind "defn", :line 225, :end-line 229, :hash "1315649154"}]}
;; clj-mutate-manifest-end
