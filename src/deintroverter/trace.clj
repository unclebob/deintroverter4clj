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

(defn- var-invoke-target? [form]
  (and (seq? form) (= 'var (first form)) (symbol? (second form))))

(defn- invoke-form? [form]
  (and (seq? form) (seq form)
       (or (symbol? (first form))
           (var-invoke-target? (first form)))))

(defn- invoke-target-sym [form]
  (when (and (seq? form) (seq form))
    (let [f (first form)]
      (cond
        (var-invoke-target? f) (second f)
        (= 'var f) (second form)
        (symbol? f) f
        :else nil))))

(defn- sym-sut-level [sym ctx]
  (let [sut (:sut ctx)]
    (cond
      (let [ns-s (fn-sym-ns sym ctx)]
        (and ns-s (contains? sut (symbol ns-s))))
      :proven

      (and (nil? (namespace sym))
           (contains? (:refer-syms ctx) sym)
           (contains? sut (get (:refer-syms ctx) sym)))
      :proven

      (and (nil? (namespace sym))
           (not (contains? (:core-syms ctx) sym))
           (seq (:refer-all-sut ctx)))
      :likely

      :else nil)))

(defn- call-sut-level [form ctx]
  (when-let [target (invoke-target-sym form)]
    (sym-sut-level target ctx)))

(defn- resolved-ns-for-sym [sym {:keys [resolve-ns all-refer-syms] :as ctx}]
  (cond
    (namespace sym) (fn-sym-ns sym ctx)
    (contains? all-refer-syms sym) (get all-refer-syms sym)
    :else nil))

(defn- explain-call [form ctx]
  (when-let [sym (invoke-target-sym form)]
    {:sym sym
     :resolved-ns (resolved-ns-for-sym sym ctx)
     :level (or (call-sut-level form ctx) :none)}))

(defn- form-children [node]
  (cond
    (seq? node) (seq node)
    (coll? node) (seq node)
    :else nil))

(defn- push-children [stack children]
  (if-let [xs (seq children)]
    (into stack (reverse (vec xs)))
    stack))

(defn- collect-calls [form]
  (loop [stack [form] calls #{}]
    (if (empty? stack)
      calls
      (let [node (peek stack)
            stack (pop stack)]
        (if-not (seq? node)
          (recur stack calls)
          (if (invoke-form? node)
            (recur (push-children stack (rest node))
                   (conj calls node))
            (recur (push-children stack (seq node)) calls)))))))

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
  (loop [f form]
    (cond
      (and (seq? f) (= '-> (first f)))  (recur (desugar-> f))
      (and (seq? f) (= '->> (first f))) (recur (desugar->> f))
      :else f)))

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

(declare trace-form)

(defn- symbols-in-form [form]
  (loop [stack [form] syms #{}]
    (if (empty? stack)
      syms
      (let [node (peek stack)
            stack (pop stack)]
        (recur (push-children stack (form-children node))
               (if (symbol? node) (conj syms node) syms))))))

(defn- binding-origin-level [sym bindings ctx tracing-syms]
  (when-not (contains? tracing-syms sym)
    (case (:verdict (trace-form (get bindings sym) bindings ctx (conj tracing-syms sym)))
      :extroverted :proven
      :likely-extroverted :likely
      nil)))

(defn- binding-origin-levels
  ([form bindings ctx]
   (binding-origin-levels form bindings ctx #{}))
  ([form bindings ctx tracing-syms]
   (keep #(binding-origin-level % bindings ctx tracing-syms)
         (filter #(contains? bindings %) (symbols-in-form form)))))

(defn- deref-form? [form]
  (and (seq? form)
       (= 'clojure.core/deref (first form))
       (symbol? (second form))))

(defn- collect-derefs [form]
  (loop [stack [form] derefs #{}]
    (if (empty? stack)
      derefs
      (let [node (peek stack)
            stack (pop stack)]
        (if-not (seq? node)
          (recur stack derefs)
          (if (deref-form? node)
            (recur (push-children stack (rest node))
                   (conj derefs node))
            (recur (push-children stack (seq node)) derefs)))))))

(defn- sut-var-ref-level [sym bindings {:keys [sut refer-syms] :as ctx}]
  (when (and (symbol? sym) (not (contains? bindings sym)))
    (cond
      (namespace sym)
      (let [ns-s (fn-sym-ns sym ctx)]
        (when (and ns-s (contains? sut (symbol ns-s)))
          :proven))

      (contains? refer-syms sym)
      (when (contains? sut (get refer-syms sym))
        :proven)

      :else nil)))

(defn- sut-var-ref-levels [form bindings ctx]
  (concat
   (keep #(sut-var-ref-level % bindings ctx)
         (symbols-in-form form))
   (keep #(sut-var-ref-level (second %) bindings ctx)
         (collect-derefs form))))

(defn- sym-test-module? [sym {:keys [test-modules] :as ctx}]
  (when-let [ns-s (resolved-ns-for-sym sym ctx)]
    (contains? test-modules (symbol ns-s))))

(defn- test-module-call-level [form ctx]
  (when-let [target (invoke-target-sym form)]
    (when (sym-test-module? target ctx)
      :proven)))

(defn- test-module-var-ref-level [sym bindings ctx]
  (when (and (symbol? sym) (not (contains? bindings sym)))
    (when (sym-test-module? sym ctx)
      :proven)))

(defn- test-module-var-ref-levels [form bindings ctx]
  (concat
   (keep #(test-module-var-ref-level % bindings ctx)
         (symbols-in-form form))
   (keep #(test-module-var-ref-level (second %) bindings ctx)
         (collect-derefs form))))

(defn reaches-test-module?
  "True when form (or do-body) calls or references a test-module namespace."
  [form bindings ctx]
  (let [expanded (expand-threading form)
        calls    (collect-calls expanded)
        levels   (concat (keep #(test-module-call-level % ctx) calls)
                         (test-module-var-ref-levels expanded bindings ctx))]
    (boolean (some #{:proven} levels))))

(defn binding-from-test-module?
  "True when any let-bound symbol originates from a test-module form."
  [bindings ctx]
  (boolean
   (some (fn [[k origin]]
           (and (symbol? k)
                (reaches-test-module? origin bindings ctx)))
         bindings)))

(defn- sut-reach-levels [form bindings ctx]
  (let [expanded (expand-threading form)
        calls    (collect-calls expanded)]
    (concat (keep #(call-sut-level % ctx) calls)
            (sut-var-ref-levels expanded bindings ctx)
            (binding-origin-levels expanded bindings ctx))))

(defn reaches-sut?
  "True when form calls or references a SUT namespace at :proven level."
  [form bindings ctx]
  (boolean (some #{:proven} (sut-reach-levels form bindings ctx))))

(defn reaches-sut-likely?
  "True when form reaches SUT at :proven or :likely (refer :all) level."
  [form bindings ctx]
  (boolean (some #{:proven :likely} (sut-reach-levels form bindings ctx))))

(defn direct-sut-invoke-form?
  "True when the outermost list form is a direct call to a SUT function.
  Unlike trace-form, does not search nested calls inside arguments."
  [form ctx]
  (boolean (call-sut-level form ctx)))

(defn- resolve-bound-form [form bindings]
  (loop [f form seen #{}]
    (if (and (symbol? f) (contains? bindings f) (not (contains? seen f)))
      (recur (get bindings f) (conj seen f))
      f)))

(defn trace-form
  "Trace a form to determine assertion verdict.
  bindings: map of symbol → originating form from let, plus optional
  :destructuring? true when a let used destructuring.
  ctx: {:sut :resolve-ns :refer-syms :refer-all-sut :core-syms}
  Returns {:verdict :extroverted|:likely-extroverted|:introverted|:questionable
           :reason keyword-or-nil}"
  ([form bindings ctx]
   (trace-form form bindings ctx #{}))
  ([form bindings ctx tracing-syms]
   (let [form (resolve-bound-form form bindings)]
     (cond
       (and (seq? form) (#{'as-> 'some-> 'some->> 'cond->} (first form)))
       {:verdict :questionable :reason :unsupported-threading-macro}

       (and (seq? form) (= 'fn (first form)))
       {:verdict :questionable :reason :anonymous-fn}

       :else
       (let [expanded (expand-threading form)
             calls    (collect-calls expanded)
             levels   (concat (keep #(call-sut-level % ctx) calls)
                              (sut-var-ref-levels expanded bindings ctx)
                              (binding-origin-levels expanded bindings ctx tracing-syms))]
         (or (levels->verdict levels)
             (when (bindings-have-destructuring? bindings)
               {:verdict :questionable :reason :destructuring})
             {:verdict :introverted :reason :no-sut-assertion}))))))

(defn explain-trace
  "Return trace detail for a form: asserted calls, binding origins, and verdict.
  bindings: let bindings active at the assertion (may include :destructuring?)."
  [form bindings ctx]
  (let [expanded (expand-threading form)
        calls    (sort-by (comp str invoke-target-sym) (collect-calls expanded))]
    {:asserted-form form
     :calls-traced (vec (keep #(explain-call % ctx) calls))
     :binding-origins
     (vec (for [sym (sort (filter #(contains? bindings %)
                                  (symbols-in-form expanded)))
               :let [origin (get bindings sym)
                     {:keys [verdict reason]} (trace-form origin bindings ctx)]]
            {:sym sym :origin origin :verdict verdict :reason reason}))
     :verdict (:verdict (trace-form form bindings ctx))
     :reason (:reason (trace-form form bindings ctx))}))

(defn make-trace-ctx
  "Build trace context from ns-info and the SUT namespace set."
  [{:keys [refer-syms refer-all] :as ns-info} sut resolve-ns & [{:keys [test-modules]}]]
  (let [sut-refer-syms (into {}
                             (keep (fn [[sym ns]]
                                     (when (contains? sut ns)
                                       [sym ns]))
                                   refer-syms))
        refer-all-sut (clojure.set/intersection sut refer-all)]
    {:sut sut
     :resolve-ns resolve-ns
     :refer-syms sut-refer-syms
     :all-refer-syms refer-syms
     :refer-all-sut refer-all-sut
     :core-syms (load-core-denylist)
     :test-modules (or test-modules #{})
     :test-ns (:namespace ns-info)
     :requires (:requires ns-info)}))