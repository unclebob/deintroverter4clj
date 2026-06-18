(ns deintroverter.trace)

(defn- call-sym? [form]
  (and (seq? form) (not= 'quote (first form)) (symbol? (first form))))

(defn- fn-sym-ns [f {:keys [resolve-ns]}]
  (if-let [n (namespace f)]
    (resolve-ns (symbol n))
    (resolve-ns f)))

(defn- sut-call? [form ctx]
  (when (call-sym? form)
    (let [f (first form)
          ns-s (fn-sym-ns f ctx)]
      (when ns-s
        (contains? (:sut ctx) (symbol ns-s))))))

(defn- collect-calls [form]
  (cond
    (not (seq? form)) #{}
    (call-sym? form)  (into #{form} (mapcat collect-calls (rest form)))
    :else             (into #{} (mapcat collect-calls form))))

(defn- desugar-> [forms]
  (loop [value (second forms) steps (drop 2 forms)]
    (if (empty? steps)
      value
      (let [step (first steps)]
        (recur (if (and (seq? step) (not= '. (first step)))
                 (cons (first step) (cons value (rest step)))
                 (list step value))
               (rest steps))))))

(defn- desugar->> [forms]
  (loop [value (second forms) steps (drop 2 forms)]
    (if (empty? steps)
      value
      (let [step (first steps)]
        (recur (if (and (seq? step) (not= '. (first step)))
                 (let [args (rest step)]
                   (cons (first step) (concat args [value])))
                 (list step value))
               (rest steps))))))

(defn- expand-threading [form]
  (cond
    (and (seq? form) (= '-> (first form)))  (expand-threading (desugar-> form))
    (and (seq? form) (= '->> (first form))) (expand-threading (desugar->> form))
    :else form))

(defn- binding-destructuring? [binding]
  (or (and (vector? binding) (some (complement symbol?) binding))
      (and (seq? binding) (not= 'quote (first binding)))))

(defn- bindings-have-destructuring? [bindings]
  (or (:destructuring? bindings)
      (some binding-destructuring? (keys bindings))))

(defn trace-form
  "Trace a form to determine assertion verdict.
  bindings: map of symbol → originating form from let, plus optional
  :destructuring? true when a let used destructuring.
  ctx: {:sut #{ns-syms} :resolve-ns fn}
  Returns {:verdict :extroverted|:introverted|:questionable :reason keyword-or-nil}"
  [form bindings ctx]
  (cond
    (and (symbol? form) (contains? bindings form))
    (trace-form (get bindings form) bindings ctx)

    (and (seq? form) (#{'as-> 'some-> 'some->> 'cond->} (first form)))
    {:verdict :questionable :reason :unsupported-threading-macro}

    (and (seq? form) (= 'fn (first form)))
    {:verdict :questionable :reason :anonymous-fn}

    :else
    (let [expanded (expand-threading form)
          calls    (collect-calls expanded)]
      (cond
        (some #(sut-call? % ctx) calls)
        {:verdict :extroverted :reason nil}

        (bindings-have-destructuring? bindings)
        {:verdict :questionable :reason :destructuring}

        :else
        {:verdict :introverted :reason :no-sut-assertion}))))