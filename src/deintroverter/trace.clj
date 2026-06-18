(ns deintroverter.trace
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn- load-core-denylist []
  (-> "deintroverter/core_sym_denylist.edn"
      io/resource
      slurp
      edn/read-string))

(defn- call-sym? [form]
  (and (seq? form) (not= 'quote (first form)) (symbol? (first form))))

(defn- fn-sym-ns [f {:keys [resolve-ns]}]
  (if-let [n (namespace f)]
    (resolve-ns (symbol n))
    (resolve-ns f)))

(defn- call-sut-level [form ctx]
  (when (call-sym? form)
    (let [f (first form)
          sut (:sut ctx)]
      (cond
        (let [ns-s (fn-sym-ns f ctx)]
          (and ns-s (contains? sut (symbol ns-s))))
        :proven

        (and (nil? (namespace f))
             (contains? (:refer-syms ctx) f)
             (contains? sut (get (:refer-syms ctx) f)))
        :proven

        (and (nil? (namespace f))
             (not (contains? (:core-syms ctx) f))
             (seq (:refer-all-sut ctx)))
        :likely

        :else nil))))

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

(defn- levels->verdict [levels]
  (cond
    (some #{:proven} levels) {:verdict :extroverted :reason nil}
    (some #{:likely} levels) {:verdict :likely-extroverted :reason :refer-all-heuristic}
    :else nil))

(defn trace-form
  "Trace a form to determine assertion verdict.
  bindings: map of symbol → originating form from let, plus optional
  :destructuring? true when a let used destructuring.
  ctx: {:sut :resolve-ns :refer-syms :refer-all-sut :core-syms}
  Returns {:verdict :extroverted|:likely-extroverted|:introverted|:questionable
           :reason keyword-or-nil}"
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
          calls    (collect-calls expanded)
          levels   (keep #(call-sut-level % ctx) calls)]
      (or (levels->verdict levels)
          (when (bindings-have-destructuring? bindings)
            {:verdict :questionable :reason :destructuring})
          {:verdict :introverted :reason :no-sut-assertion}))))

(defn make-trace-ctx
  "Build trace context from ns-info and the SUT namespace set."
  [{:keys [refer-syms refer-all] :as _ns-info} sut resolve-ns]
  (let [refer-syms (into {}
                         (keep (fn [[sym ns]]
                                 (when (contains? sut ns)
                                   [sym ns]))
                               refer-syms))
        refer-all-sut (clojure.set/intersection sut refer-all)]
    {:sut sut
     :resolve-ns resolve-ns
     :refer-syms refer-syms
     :refer-all-sut refer-all-sut
     :core-syms (load-core-denylist)}))