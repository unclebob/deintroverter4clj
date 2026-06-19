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
         (process-forms body new-bindings trace-ctx))

       (#{'do 'try 'catch 'finally} (first form))
       (process-forms (rest form) bindings trace-ctx)

       (= 'with-redefs (first form))
       (process-forms (drop 2 form) bindings trace-ctx)

       (= 'doseq (first form))
       (process-doseq form bindings trace-ctx)

       (= 'fn (first form))
       (process-forms (drop 2 form) bindings trace-ctx)

       :else
       (or (process-fn-invoke form bindings trace-ctx)
           (let [parsed (assertions/parse-assertion form)]
             (if parsed
               (let [{:keys [asserted-form reason]} parsed]
                 (if reason
                   [{:verdict :questionable :reason reason}]
                   [(trace/trace-form asserted-form bindings trace-ctx)]))
               (process-forms (rest form) bindings trace-ctx))))))
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