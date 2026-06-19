(ns deintroverter.analyze
  (:require [deintroverter.parse :as parse]
            [deintroverter.assertions :as assertions]
            [deintroverter.test-modules :as test-modules]
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

(defn- simple-vector-pattern? [pattern]
  (and (vector? pattern) (seq pattern)
       (not (some #(= '& %) pattern))
       (every? symbol? pattern)))

(defn- vector-rest-pattern? [pattern]
  (and (vector? pattern)
       (let [idx (.indexOf pattern '&)]
         (and (not (neg? idx))
              (= (inc idx) (dec (count pattern)))
              (symbol? (nth pattern (inc idx)))
              (every? symbol? (subvec pattern 0 idx))))))

(defn- supported-vector-pattern? [pattern]
  (or (simple-vector-pattern? pattern)
      (vector-rest-pattern? pattern)))

(def ^:private map-destructure-keys
  #{:keys :syms :strs :as :or})

(defn- symbol-vector? [v]
  (and (vector? v) (every? symbol? v)))

(defn- supported-map-pattern? [pattern]
  (and (map? pattern)
       (every? map-destructure-keys (keys pattern))
       (every? symbol-vector? (keep #(get pattern %) [:keys :syms :strs]))
       (or (nil? (:as pattern)) (symbol? (:as pattern)))
       (or (nil? (:or pattern)) (map? (:or pattern)))))

(defn- supported-destructure-pattern? [pattern]
  (or (supported-vector-pattern? pattern)
      (supported-map-pattern? pattern)))

(defn- unsupported-destructure-pattern? [pattern]
  (and (destructuring-binding? pattern)
       (not (supported-destructure-pattern? pattern))))

(defn- nth-binding [rhs i]
  (if (vector? rhs) (nth rhs i) `(nth ~rhs ~i)))

(defn- drop-binding [rhs n]
  (if (vector? rhs) (vec (drop n rhs)) `(drop ~n ~rhs)))

(defn- get-binding [rhs key]
  (if (and (map? rhs) (contains? rhs key)) (get rhs key) `(get ~rhs ~key)))

(defn- with-or-default [expr sym or-map]
  (if-let [default (get or-map sym)]
    `(or ~expr ~default)
    expr))

(defn- bind-vector-positions [syms rhs bindings]
  (reduce (fn [b [i sym]]
            (assoc b sym (nth-binding rhs i)))
          bindings
          (map-indexed vector syms)))

(defn- expand-vector-destructure [pattern rhs bindings]
  (let [ampersand-idx (.indexOf pattern '&)]
    (if (neg? ampersand-idx)
      (bind-vector-positions pattern rhs bindings)
      (let [fixed (subvec pattern 0 ampersand-idx)
            rest-sym (nth pattern (inc ampersand-idx))
            bindings (assoc bindings rest-sym (drop-binding rhs (count fixed)))]
        (bind-vector-positions fixed rhs bindings)))))

(defn- expand-map-destructure [pattern rhs bindings]
  (let [or-map (:or pattern)]
    (reduce-kv
     (fn [b k v]
       (case k
         :keys (reduce (fn [b2 sym]
                         (let [expr (get-binding rhs (keyword (name sym)))]
                           (assoc b2 sym (with-or-default expr sym or-map))))
                       b v)
         :syms (reduce (fn [b2 sym]
                         (let [expr (get-binding rhs (list 'quote sym))]
                           (assoc b2 sym (with-or-default expr sym or-map))))
                       b v)
         :strs (reduce (fn [b2 sym]
                         (let [expr (get-binding rhs (name sym))]
                           (assoc b2 sym (with-or-default expr sym or-map))))
                       b v)
         :as (assoc b v rhs)
         :or b
         b))
     bindings
     pattern)))

(defn- expand-destructure-bindings [pattern rhs bindings]
  (cond
    (symbol? pattern) (assoc bindings pattern rhs)
    (supported-vector-pattern? pattern) (expand-vector-destructure pattern rhs bindings)
    (supported-map-pattern? pattern) (expand-map-destructure pattern rhs bindings)
    :else bindings))

(defn- fn-form? [form]
  (and (seq? form) (#{'fn 'fn*} (first form))))

(defn- fn-param-syms [fn-form]
  (let [params (second fn-form)]
    (if (vector? params) (vec params) (vector (first params)))))

(defn- doseq-coll [coll-expr bindings]
  (cond
    (vector? coll-expr) coll-expr
    (and (symbol? coll-expr) (contains? bindings coll-expr)) (get bindings coll-expr)
    :else nil))

(declare process-forms)

(defn- bindings-for-doseq-item [bind-expr item bindings]
  (cond
    (symbol? bind-expr)
    (assoc bindings bind-expr item)

    (supported-destructure-pattern? bind-expr)
    (expand-destructure-bindings bind-expr item bindings)

    :else bindings))

(defn- process-doseq [form bindings trace-ctx]
  (let [binding-form (second form)
        body (drop 2 form)]
    (if-not (and (vector? binding-form) (= 2 (count binding-form)))
      (process-forms body bindings trace-ctx)
      (let [[bind-expr coll-expr] binding-form
            coll (doseq-coll coll-expr bindings)]
        (if (vector? coll)
          (mapcat (fn [item]
                    (process-forms body
                                   (bindings-for-doseq-item bind-expr item bindings)
                                   trace-ctx))
                  coll)
          (process-forms body bindings trace-ctx))))))

(defn- process-fn-invoke [form bindings trace-ctx]
  (when (and (seq? form) (symbol? (first form)) (seq (rest form)))
    (when-let [fn-form (get bindings (first form))]
      (when (fn-form? fn-form)
        (let [params (fn-param-syms fn-form)
              body (drop 2 fn-form)
              new-bindings (merge bindings (zipmap params (rest form)))]
          (process-forms body new-bindings trace-ctx))))))

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

(defn- sut-invoke-form? [form _bindings trace-ctx]
  (trace/direct-sut-invoke-form? form trace-ctx))

(defn- assertion-result
  [form bindings trace-ctx]
  (let [{:keys [verdict reason]} (trace/trace-form form bindings trace-ctx)]
    {:verdict verdict
     :reason reason
     :trace (trace/explain-trace form bindings trace-ctx)}))

(defn- stub-assertion-result
  [assertion-form bindings trace-ctx preceding-sut]
  (let [trace-target (or preceding-sut assertion-form)
        {:keys [verdict reason]} (trace/trace-form trace-target bindings trace-ctx)
        trace (assoc (trace/explain-trace trace-target bindings trace-ctx)
                     :assertion-form assertion-form
                     :preceding-sut-call preceding-sut)]
    {:verdict verdict :reason reason :trace trace}))

(defn- questionable-result [form reason]
  {:verdict :questionable
   :reason reason
   :trace {:assertion-form form}})

(defn- build-finding-trace [trace-ctx sut assertion-results]
  {:test-ns (:test-ns trace-ctx)
   :requires (:requires trace-ctx)
   :refer-syms (:all-refer-syms trace-ctx)
   :sut-namespaces sut
   :assertions (vec (map :trace assertion-results))})

(defn- let-bindings [form bindings]
  (let [after-bindings (rest form)
        binding-form   (first after-bindings)
        body           (rest after-bindings)
        pairs (if (vector? binding-form)
                (partition 2 binding-form)
                (partition 2 after-bindings))
        unsupported-destructure?
        (some (fn [[k _]] (unsupported-destructure-pattern? k)) pairs)
        new-bindings (reduce (fn [b [k v]]
                               (cond
                                 (symbol? k) (assoc b k v)
                                 (supported-destructure-pattern? k)
                                 (expand-destructure-bindings k v b)
                                 :else b))
                             bindings pairs)
        new-bindings (if unsupported-destructure?
                       (assoc new-bindings :destructuring? true)
                       new-bindings)]
    {:body body :bindings new-bindings}))

(declare process-forms-sequential)

(defn- process-one-form [form bindings trace-ctx preceding]
  (cond
    (not (seq? form))
    {:results [] :preceding preceding}

    (= 'let (first form))
    (let [{:keys [body bindings]} (let-bindings form bindings)]
      (assoc (process-forms-sequential body bindings trace-ctx nil)
             :preceding preceding))

    (#{'do 'try 'catch 'finally} (first form))
    (assoc (process-forms-sequential (rest form) bindings trace-ctx preceding)
           :preceding preceding)

    (= 'with-redefs (first form))
    (assoc (process-forms-sequential (drop 2 form) bindings trace-ctx preceding)
           :preceding preceding)

    (= 'doseq (first form))
    {:results (process-doseq form bindings trace-ctx) :preceding preceding}

    (= 'fn (first form))
    (assoc (process-forms-sequential (drop 2 form) bindings trace-ctx nil)
           :preceding preceding)

    :else
    (or (when-let [results (process-fn-invoke form bindings trace-ctx)]
          {:results results :preceding preceding})
        (let [parsed (assertions/parse-assertion form)]
          (cond
            (and parsed (assertions/stub-invocation? parsed))
            {:results [(stub-assertion-result form bindings trace-ctx preceding)]
             :preceding preceding}

            (and parsed (:reason parsed))
            {:results [(questionable-result form (:reason parsed))]
             :preceding preceding}

            parsed
            {:results [(assertion-result (:asserted-form parsed) bindings trace-ctx)]
             :preceding preceding}

            (sut-invoke-form? form bindings trace-ctx)
            {:results [] :preceding form}

            :else
            (assoc (process-forms-sequential (rest form) bindings trace-ctx nil)
                   :preceding preceding))))))

(defn- process-forms-sequential [forms bindings trace-ctx preceding]
  (loop [forms (seq forms), bindings bindings, preceding preceding, results []]
    (if (empty? forms)
      {:results results :preceding preceding}
      (let [step (process-one-form (first forms) bindings trace-ctx preceding)]
        (recur (rest forms) bindings (:preceding step) (into results (:results step)))))))

(defn- process-forms [forms bindings trace-ctx]
  (:results (process-forms-sequential forms bindings trace-ctx nil)))

(defn- body-form [body]
  (if (= 1 (count body))
    (first body)
    (cons 'do body)))

(defn- promote-cloistered [verdict body bindings trace-ctx]
  (if (and (= :introverted (:verdict verdict))
           (trace/reaches-test-module? (body-form body) bindings trace-ctx))
    {:verdict :cloistered :reason :reaches-test-module}
    verdict))

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
  "Analyze a test file path. opts: {:sut #{namespace-syms} :project-ctx map}
  Returns vector of finding maps."
  [file-path {:keys [sut project-ctx]}]
  (let [content (slurp file-path)
        forms   (parse/read-string-all content)
        ns-form (first forms)
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
        tests   (find-tests forms)]
    (vec
     (for [{:keys [form test-name body line]} tests
           :let [results (vec (process-forms body {} trace-ctx))
                 {:keys [verdict reason]}
                 (promote-cloistered (test-verdict results) body {} trace-ctx)]]
       {:file file-path
        :line line
        :test-name (if (string? test-name) test-name (name test-name))
        :test-form form
        :verdict verdict
        :reason reason
        :sut-namespaces sut
        :trace (assoc (build-finding-trace trace-ctx sut results)
                      :test-modules test-modules)}))))