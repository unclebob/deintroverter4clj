(ns deintroverter.trace
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn- load-core-denylist []
  (-> "deintroverter/core_sym_denylist.edn"
      io/resource
      slurp
      edn/read-string))

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

(defn- invoke-target-from-head [f form]
  (or (when (var-invoke-target? f) (second f))
      (when (= 'var f) (second form))
      (when (symbol? f) f)))

(defn- invoke-target-sym [form]
  (when (and (seq? form) (seq form))
    (invoke-target-from-head (first form) form)))

(defn- namespaced-sut-level [sym ctx]
  (let [ns-s (fn-sym-ns sym ctx)]
    (when (and ns-s (contains? (:sut ctx) (symbol ns-s)))
      :proven)))

(defn- referred-sut-level [sym ctx]
  (when (and (nil? (namespace sym))
             (contains? (:refer-syms ctx) sym)
             (contains? (:sut ctx) (get (:refer-syms ctx) sym)))
    :proven))

(defn- refer-all-sut-level [sym ctx]
  (when (and (nil? (namespace sym))
             (not (contains? (:core-syms ctx) sym))
             (seq (:refer-all-sut ctx)))
    :likely))

(defn- sym-sut-level [sym ctx]
  (or (namespaced-sut-level sym ctx)
      (referred-sut-level sym ctx)
      (refer-all-sut-level sym ctx)))

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

(def ^:private threading-expanders
  {'-> desugar->
   '->> desugar->>})

(defn- expand-threading [form]
  (loop [f form]
    (if-let [expand (when (seq? f) (get threading-expanders (first f)))]
      (recur (expand f))
      f)))

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
            stack (pop stack)
            syms (if (symbol? node) (conj syms node) syms)]
        (recur (push-children stack (form-children node)) syms)))))

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

(defn- namespaced-var-ref-level [sym ctx]
  (let [ns-s (fn-sym-ns sym ctx)]
    (when (and ns-s (contains? (:sut ctx) (symbol ns-s)))
      :proven)))

(defn- referred-var-ref-level [sym {:keys [sut refer-syms]}]
  (when (contains? refer-syms sym)
    (when (contains? sut (get refer-syms sym))
      :proven)))

(defn- sut-var-ref-level [sym bindings ctx]
  (when (and (symbol? sym) (not (contains? bindings sym)))
    (or (when (namespace sym) (namespaced-var-ref-level sym ctx))
        (referred-var-ref-level sym ctx))))

(defn- sym-and-deref-levels [level-fn form bindings ctx]
  (concat
   (keep #(level-fn % bindings ctx) (symbols-in-form form))
   (keep #(level-fn (second %) bindings ctx) (collect-derefs form))))

(defn- sut-var-ref-levels [form bindings ctx]
  (sym-and-deref-levels sut-var-ref-level form bindings ctx))

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
  (sym-and-deref-levels test-module-var-ref-level form bindings ctx))

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

(def ^:private questionable-form-reasons
  #{'as-> 'some-> 'some->> 'cond->})

(defn- questionable-form? [form]
  (and (seq? form)
       (or (contains? questionable-form-reasons (first form))
           (= 'fn (first form)))))

(defn- unsupported-threading-result [_form]
  {:verdict :questionable :reason :unsupported-threading-macro})

(defn- anonymous-fn-result [_form]
  {:verdict :questionable :reason :anonymous-fn})

(def ^:private questionable-form-results
  {'as-> unsupported-threading-result
   'some-> unsupported-threading-result
   'some->> unsupported-threading-result
   'cond-> unsupported-threading-result
   'fn anonymous-fn-result})

(defn- questionable-form-result [form]
  (when-let [handler (get questionable-form-results (first form))]
    (handler form)))

(defn- traced-levels [form bindings ctx tracing-syms]
  (let [expanded (expand-threading form)
        calls (collect-calls expanded)]
    (concat (keep #(call-sut-level % ctx) calls)
            (sut-var-ref-levels expanded bindings ctx)
            (binding-origin-levels expanded bindings ctx tracing-syms))))

(defn- verdict-from-levels [levels bindings]
  (or (levels->verdict levels)
      (when (bindings-have-destructuring? bindings)
        {:verdict :questionable :reason :destructuring})
      {:verdict :introverted :reason :no-sut-assertion}))

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
     (if (questionable-form? form)
       (questionable-form-result form)
       (verdict-from-levels (traced-levels form bindings ctx tracing-syms) bindings)))))

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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-19T12:53:20.908057-05:00", :module-hash "765203333", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "-1660410122"} {:id "defn-/load-core-denylist", :kind "defn-", :line 5, :end-line 9, :hash "538025892"} {:id "defn-/fn-sym-ns", :kind "defn-", :line 11, :end-line 14, :hash "1860371223"} {:id "defn-/var-invoke-target?", :kind "defn-", :line 16, :end-line 17, :hash "-1269856556"} {:id "defn-/invoke-form?", :kind "defn-", :line 19, :end-line 22, :hash "-1309411709"} {:id "defn-/invoke-target-from-head", :kind "defn-", :line 24, :end-line 27, :hash "587155424"} {:id "defn-/invoke-target-sym", :kind "defn-", :line 29, :end-line 31, :hash "1965957781"} {:id "defn-/namespaced-sut-level", :kind "defn-", :line 33, :end-line 36, :hash "-1891662282"} {:id "defn-/referred-sut-level", :kind "defn-", :line 38, :end-line 42, :hash "-157927174"} {:id "defn-/refer-all-sut-level", :kind "defn-", :line 44, :end-line 48, :hash "-815051651"} {:id "defn-/sym-sut-level", :kind "defn-", :line 50, :end-line 53, :hash "-1607828535"} {:id "defn-/call-sut-level", :kind "defn-", :line 55, :end-line 57, :hash "1250162350"} {:id "defn-/resolved-ns-for-sym", :kind "defn-", :line 59, :end-line 63, :hash "936319648"} {:id "defn-/explain-call", :kind "defn-", :line 65, :end-line 69, :hash "885863234"} {:id "defn-/form-children", :kind "defn-", :line 71, :end-line 75, :hash "-152447756"} {:id "defn-/push-children", :kind "defn-", :line 77, :end-line 80, :hash "183488808"} {:id "defn-/collect-calls", :kind "defn-", :line 82, :end-line 93, :hash "-648811333"} {:id "defn-/desugar->", :kind "defn-", :line 95, :end-line 103, :hash "-1089437132"} {:id "defn-/desugar->>", :kind "defn-", :line 105, :end-line 114, :hash "-1337162929"} {:id "def/threading-expanders", :kind "def", :line 116, :end-line 118, :hash "1934965324"} {:id "defn-/expand-threading", :kind "defn-", :line 120, :end-line 124, :hash "1358340665"} {:id "defn-/binding-destructuring?", :kind "defn-", :line 126, :end-line 128, :hash "681657584"} {:id "defn-/bindings-have-destructuring?", :kind "defn-", :line 130, :end-line 132, :hash "1715671843"} {:id "defn-/levels->verdict", :kind "defn-", :line 134, :end-line 138, :hash "-26690889"} {:id "form/24/declare", :kind "declare", :line 140, :end-line 140, :hash "535213460"} {:id "defn-/symbols-in-form", :kind "defn-", :line 142, :end-line 149, :hash "-873682114"} {:id "defn-/binding-origin-level", :kind "defn-", :line 151, :end-line 156, :hash "684688370"} {:id "defn-/binding-origin-levels", :kind "defn-", :line 158, :end-line 163, :hash "1205837488"} {:id "defn-/deref-form?", :kind "defn-", :line 165, :end-line 168, :hash "-13340623"} {:id "defn-/collect-derefs", :kind "defn-", :line 170, :end-line 181, :hash "-2007463881"} {:id "defn-/namespaced-var-ref-level", :kind "defn-", :line 183, :end-line 186, :hash "-1997436509"} {:id "defn-/referred-var-ref-level", :kind "defn-", :line 188, :end-line 191, :hash "158070887"} {:id "defn-/sut-var-ref-level", :kind "defn-", :line 193, :end-line 196, :hash "797974342"} {:id "defn-/sym-and-deref-levels", :kind "defn-", :line 198, :end-line 201, :hash "-1412294122"} {:id "defn-/sut-var-ref-levels", :kind "defn-", :line 203, :end-line 204, :hash "-326968250"} {:id "defn-/sym-test-module?", :kind "defn-", :line 206, :end-line 208, :hash "-1739061437"} {:id "defn-/test-module-call-level", :kind "defn-", :line 210, :end-line 213, :hash "-998454715"} {:id "defn-/test-module-var-ref-level", :kind "defn-", :line 215, :end-line 218, :hash "1815167385"} {:id "defn-/test-module-var-ref-levels", :kind "defn-", :line 220, :end-line 221, :hash "533224013"} {:id "defn/reaches-test-module?", :kind "defn", :line 223, :end-line 230, :hash "839689982"} {:id "defn/binding-from-test-module?", :kind "defn", :line 232, :end-line 239, :hash "-1683734065"} {:id "defn-/sut-reach-levels", :kind "defn-", :line 241, :end-line 246, :hash "2070993866"} {:id "defn/reaches-sut?", :kind "defn", :line 248, :end-line 251, :hash "-1678241531"} {:id "defn/reaches-sut-likely?", :kind "defn", :line 253, :end-line 256, :hash "-97519051"} {:id "defn/direct-sut-invoke-form?", :kind "defn", :line 258, :end-line 262, :hash "-1431562663"} {:id "defn-/resolve-bound-form", :kind "defn-", :line 264, :end-line 268, :hash "1388745293"} {:id "def/questionable-form-reasons", :kind "def", :line 270, :end-line 271, :hash "809333892"} {:id "defn-/questionable-form?", :kind "defn-", :line 273, :end-line 276, :hash "-1731260804"} {:id "defn-/unsupported-threading-result", :kind "defn-", :line 278, :end-line 279, :hash "907190371"} {:id "defn-/anonymous-fn-result", :kind "defn-", :line 281, :end-line 282, :hash "-836305096"} {:id "def/questionable-form-results", :kind "def", :line 284, :end-line 289, :hash "-231524056"} {:id "defn-/questionable-form-result", :kind "defn-", :line 291, :end-line 293, :hash "830550921"} {:id "defn-/traced-levels", :kind "defn-", :line 295, :end-line 300, :hash "1191244849"} {:id "defn-/verdict-from-levels", :kind "defn-", :line 302, :end-line 306, :hash "1625454981"} {:id "defn/trace-form", :kind "defn", :line 308, :end-line 321, :hash "1917614390"} {:id "defn/explain-trace", :kind "defn", :line 323, :end-line 338, :hash "923795746"} {:id "defn/make-trace-ctx", :kind "defn", :line 340, :end-line 357, :hash "-2008006516"}]}
;; clj-mutate-manifest-end
