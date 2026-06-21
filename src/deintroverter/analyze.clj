(ns deintroverter.analyze
  (:require [deintroverter.parse :as parse]
            [deintroverter.assertions :as assertions]
            [deintroverter.test-modules :as test-modules]
            [deintroverter.trace :as trace]))

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

(defn- destructuring-binding? [k]
  (not (symbol? k)))

(declare supported-vector-pattern?)

(defn- vector-pattern-element? [elem]
  (or (symbol? elem) (supported-vector-pattern? elem)))

(defn- vector-rest-index [pattern]
  (.indexOf pattern '&))

(defn- supported-vector-pattern? [pattern]
  (and (vector? pattern) (seq pattern)
       (let [ampersand-idx (vector-rest-index pattern)]
         (if (neg? ampersand-idx)
           (every? vector-pattern-element? pattern)
           (and (= (inc ampersand-idx) (dec (count pattern)))
                (symbol? (nth pattern (inc ampersand-idx)))
                (every? vector-pattern-element? (subvec pattern 0 ampersand-idx)))))))

(def ^:private map-destructure-keys
  #{:keys :syms :strs :as :or})

(defn- symbol-vector? [v]
  (and (vector? v) (every? symbol? v)))

(defn- standard-map-destructure-pattern? [pattern]
  (and (map? pattern)
       (every? map-destructure-keys (keys pattern))
       (every? symbol-vector? (keep #(get pattern %) [:keys :syms :strs]))
       (or (nil? (:as pattern)) (symbol? (:as pattern)))
       (or (nil? (:or pattern)) (map? (:or pattern)))))

(defn- symbol-key-map-pattern? [pattern]
  (and (map? pattern) (seq pattern) (every? symbol? (keys pattern))))

(defn- supported-map-pattern? [pattern]
  (or (standard-map-destructure-pattern? pattern)
      (symbol-key-map-pattern? pattern)))

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

(declare expand-vector-destructure)

(defn- expand-vector-element [elem rhs bindings]
  (if (symbol? elem)
    (assoc bindings elem rhs)
    (expand-vector-destructure elem rhs bindings)))

(defn- expand-vector-destructure [pattern rhs bindings]
  (let [ampersand-idx (vector-rest-index pattern)]
    (if (neg? ampersand-idx)
      (reduce (fn [b [i elem]]
                (expand-vector-element elem (nth-binding rhs i) b))
              bindings
              (map-indexed vector pattern))
      (let [fixed (subvec pattern 0 ampersand-idx)
            rest-sym (nth pattern (inc ampersand-idx))
            bindings (assoc bindings rest-sym (drop-binding rhs (count fixed)))]
        (reduce (fn [b [i elem]]
                  (expand-vector-element elem (nth-binding rhs i) b))
                bindings
                (map-indexed vector fixed))))))

(defn- map-lookup-key [key-spec]
  (if (keyword? key-spec) key-spec (list 'quote key-spec)))

(defn- expand-symbol-key-map-destructure [pattern rhs bindings]
  (reduce-kv
   (fn [b sym key-spec]
     (assoc b sym (get-binding rhs (map-lookup-key key-spec))))
   bindings
   pattern))

(defn- bind-map-keys [syms rhs or-map bindings key-fn]
  (reduce (fn [b sym]
            (let [expr (get-binding rhs (key-fn sym))]
              (assoc b sym (with-or-default expr sym or-map))))
          bindings
          syms))

(defn- bind-map-entry [k v rhs or-map bindings]
  (get {:keys (bind-map-keys v rhs or-map bindings #(keyword (name %)))
        :syms (bind-map-keys v rhs or-map bindings #(list 'quote %))
        :strs (bind-map-keys v rhs or-map bindings #(name %))
        :as (assoc bindings v rhs)
        :or bindings}
       k bindings))

(defn- expand-map-destructure [pattern rhs bindings]
  (if (symbol-key-map-pattern? pattern)
    (expand-symbol-key-map-destructure pattern rhs bindings)
    (let [or-map (:or pattern)]
      (reduce-kv (fn [b k v] (bind-map-entry k v rhs or-map b))
                 bindings
                 pattern))))

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

(defn- literal-true? [x] (= x true))
(defn- literal-false? [x] (= x false))

(defn- assertion-form? [form]
  (boolean (assertions/parse-assertion form)))

(declare contains-assertion?)

(defn- nested-forms-contain-assertion? [form drop-n]
  (boolean (some contains-assertion? (drop drop-n form))))

(defn- do-form-contains-assertion? [form]
  (and (seq? form) (= 'do (first form)) (nested-forms-contain-assertion? form 1)))

(defn- binding-form-contains-assertion? [form]
  (and (seq? form) (#{'let 'loop 'binding} (first form)) (>= (count form) 3)
       (nested-forms-contain-assertion? form 2)))

(defn- contains-assertion? [form]
  (cond
    (nil? form) false
    (assertion-form? form) true
    (do-form-contains-assertion? form) true
    (binding-form-contains-assertion? form) true
    :else false))

(def ^:private non-empty-coll-heads #{'seq 'not-empty 'empty?})

(defn- non-empty-coll-expr [form]
  (when (and (seq? form)
             (= 2 (count form))
             (non-empty-coll-heads (first form)))
    (second form)))

(defn- seq-not-empty-guard? [macro asserted coll-sym coll-expr]
  (and (= coll-sym coll-expr)
       (contains? #{:should :is :are} macro)
       (seq? asserted)
       (#{'seq 'not-empty} (first asserted))))

(defn- empty?-guard? [macro asserted coll-sym coll-expr]
  (and (= coll-sym coll-expr)
       (contains? #{:should-not :should-not-be} macro)
       (seq? asserted)
       (= 'empty? (first asserted))))

(defn- should-be-nil-guard? [macro asserted coll-sym]
  (and (= macro :should-be-nil) (= asserted coll-sym)))

(defn- non-empty-guard-assertion? [form coll-sym]
  (when-let [parsed (assertions/parse-assertion form)]
    (let [asserted (:asserted-form parsed)
          macro (:macro parsed)
          coll-expr (non-empty-coll-expr asserted)]
      (or (seq-not-empty-guard? macro asserted coll-sym coll-expr)
          (empty?-guard? macro asserted coll-sym coll-expr)
          (should-be-nil-guard? macro asserted coll-sym)))))

(defn- preceded-by-non-empty-guard? [done-forms coll-expr]
  (when (symbol? coll-expr)
    (boolean
     (some #(non-empty-guard-assertion? % coll-expr)
           (take-last 2 done-forms)))))

(defn- flattenable-doseq-coll? [coll-expr bindings]
  (vector? (doseq-coll coll-expr bindings)))

(def ^:private empty-cctx {:depth 0 :causes []})

(defn- ctx-depth [cctx] (:depth cctx 0))

(defn- push-cctx-cause [cctx cause context]
  {:depth (inc (ctx-depth cctx))
   :causes (conj (:causes cctx []) {:cause cause :context context})})

(defn- innermost-cctx-cause [cctx]
  (when (pos? (ctx-depth cctx))
    (last (:causes cctx))))

(defn- symbol-in-form? [sym form]
  (cond
    (= form sym) true
    (seq? form) (boolean (some #(symbol-in-form? sym %) form))
    :else false))

(defn- near-doseq-guard? [done-forms coll-expr]
  (when (symbol? coll-expr)
    (boolean
     (some (fn [form]
             (and (assertion-form? form)
                  (not (non-empty-guard-assertion? form coll-expr))
                  (symbol-in-form? coll-expr form)))
           (take-last 2 done-forms)))))

(defn- doseq-conditional-cctx [coll-expr bindings done-forms cctx]
  (cond
    (or (flattenable-doseq-coll? coll-expr bindings)
        (preceded-by-non-empty-guard? done-forms coll-expr))
    cctx

    (symbol? coll-expr)
    (push-cctx-cause cctx
                     (if (near-doseq-guard? done-forms coll-expr)
                       :near-doseq-guard
                       :missing-doseq-guard)
                     {:head 'doseq :coll coll-expr})

    :else
    (push-cctx-cause cctx :non-flattenable-doseq {:head 'doseq :coll coll-expr})))

(defn- partial-dispatch-if? [form]
  (when (and (seq? form) (= 'if (first form)))
    (let [[_ _ then else] form
          then-a (contains-assertion? then)
          else-a (contains-assertion? else)]
      (and (or then-a else-a) (not (and then-a else-a))))))

(defn- runtime-conditional-cause [form]
  (if (partial-dispatch-if? form)
    {:cause :partial-dispatch-if :context {:head 'if}}
    {:cause :runtime-conditional :context {:head (first form)}}))

(defn- reducible-cond-step [test branches remaining]
  (cond
    (= :else test) {:done (seq (conj branches (second remaining)))}
    (literal-true? test) {:done (list (second remaining))}
    (literal-false? test) {:skip true}
    :else {:abort true}))

(defn- reducible-cond-iterate [remaining branches]
  (let [{:keys [done skip abort]} (reducible-cond-step (first remaining) branches remaining)]
    (cond
      abort :abort
      done {:done done}
      skip {:continue (drop 2 remaining) :branches branches})))

(defn- reducible-cond-branches [clauses]
  (loop [remaining (seq clauses) branches []]
    (if (empty? remaining)
      (seq branches)
      (let [step (reducible-cond-iterate remaining branches)]
        (cond
          (= step :abort) nil
          (:done step) (:done step)
          :else (recur (:continue step) (:branches step)))))))

(defn- reducible-unary-conditional-branches
  [form {:keys [head skip? active?]}]
  (when (= head (first form))
    (let [test (second form)]
      (cond
        (skip? test) []
        (active? test) (rest form)
        :else nil))))

(defn- reducible-if-literal-branches [form]
  (when (= 'if (first form))
    (let [[_ test then else] form]
      (cond
        (literal-false? test) (remove nil? [else])
        (literal-true? test) (remove nil? [then])
        :else nil))))

(def ^:private not-literal-sentinel ::not-literal)

(defn- literal-case-dispatch-value? [v]
  (or (keyword? v) (string? v) (number? v) (boolean? v) (char? v) (symbol? v) (nil? v)))

(defn- resolve-literal-value [expr bindings]
  (let [value (cond
                (symbol? expr)
                (if (contains? bindings expr)
                  (get bindings expr)
                  not-literal-sentinel)

                (literal-case-dispatch-value? expr)
                expr

                :else not-literal-sentinel)]
    (if (literal-case-dispatch-value? value)
      value
      not-literal-sentinel)))

(defn- case-matching-expr [dispatch-val clauses]
  (when (seq clauses)
    (let [n (count clauses)
          has-default? (odd? n)
          pair-count (if has-default? (dec n) n)
          pairs (partition 2 2 (subvec clauses 0 pair-count))
          default (when has-default? (last clauses))]
      (or (some (fn [[k expr]] (when (= dispatch-val k) expr)) pairs)
          default))))

(defn- reducible-case-literal-branches [form bindings]
  (when (#{'case 'case+} (first form))
    (let [dispatch-val (resolve-literal-value (second form) bindings)]
      (when-not (= dispatch-val not-literal-sentinel)
        (when-let [expr (case-matching-expr dispatch-val (vec (drop 2 form)))]
          [expr])))))

(defn- reducible-literal-branches [form]
  (when (seq? form)
    (or (reducible-unary-conditional-branches form
                                              {:head 'when
                                               :skip? literal-false?
                                               :active? literal-true?})
        (reducible-unary-conditional-branches form
                                              {:head 'when-not
                                               :skip? literal-true?
                                               :active? literal-false?})
        (reducible-if-literal-branches form)
        (when (= 'cond (first form))
          (reducible-cond-branches (rest form))))))

(defn- dispatch-if-branches [form]
  (when (and (seq? form) (= 'if (first form)))
    (let [[_ _ then else] form]
      (when (and (contains-assertion? then) (contains-assertion? else))
        (remove nil? [then else])))))

(defn- reducible-conditional-branches [form bindings]
  (or (reducible-literal-branches form)
      (reducible-case-literal-branches form bindings)
      (dispatch-if-branches form)))

(def ^:private conditional-head-syms
  #{'when 'when-not 'if 'cond 'case 'case+ 'condp 'and 'or
    'if-let 'when-let 'when-some 'if-some 'while})

(defn- cond-branch-forms [clauses]
  (loop [remaining (seq clauses), branches []]
    (if (empty? remaining)
      branches
      (let [test (first remaining)]
        (if (= :else test)
          (conj branches (second remaining))
          (recur (drop 2 remaining) (conj branches (second remaining))))))))

(defn- case-branch-forms [form]
  (let [clauses (vec (drop 2 form))]
    (if (empty? clauses)
      []
      (if (odd? (count clauses))
        (into (map second (partition 2 2 (subvec clauses 0 (dec (count clauses)))))
              [(last clauses)])
        (map second (partition 2 2 clauses))))))

(defn- condp-branch-forms [form]
  (map second (partition 2 2 (drop 3 form))))

(defn- if-branch-forms [form]
  (let [parts (rest form)]
    (remove nil? [(nth parts 1 nil) (nth parts 2 nil)])))

(def ^:private conditional-branch-extractors
  {'when (fn [form] (drop 1 form))
   'when-not (fn [form] (drop 1 form))
   'if if-branch-forms
   'cond (fn [form] (cond-branch-forms (rest form)))
   'case case-branch-forms
   'case+ case-branch-forms
   'condp condp-branch-forms
   'and (fn [form] (rest form))
   'or (fn [form] (rest form))
   'if-let (fn [form] (drop 2 form))
   'when-let (fn [form] (drop 2 form))
   'when-some (fn [form] (drop 2 form))
   'if-some (fn [form] (drop 2 form))
   'while (fn [form] (list (nth form 2)))})

(defn- conditional-branch-forms [form]
  (when-let [extract (get conditional-branch-extractors (first form))]
    (when (and (seq? form) (contains? conditional-head-syms (first form)))
      (extract form))))

(declare process-forms-sequential)

(defn- process-frame
  [{:keys [todo bindings ws cctx resume complete]}]
  {:todo (seq todo) :bindings bindings :ws ws :cctx cctx :resume resume :complete complete})

(defn- complete-preceding [{:keys [ws resume complete]}]
  (get {:child (:preceding ws)
        :resume (:preceding (:ws resume))}
       (:preceding complete)
       (:preceding complete)))

(defn- complete-seen-sut? [{:keys [ws resume complete]}]
  (get {:child (:seen-sut? ws)
        :resume (:seen-sut? (:ws resume))
        :merge-or (or (:seen-sut? (:ws resume)) (:seen-sut? ws))}
       (:seen-sut? complete)
       false))

(defn- complete-child-ws [child-frame]
  {:preceding (complete-preceding child-frame)
   :seen-sut? (complete-seen-sut? child-frame)})

(defn- push-child-frame [stack child]
  (let [parent (peek stack)
        resume (process-frame (assoc parent :todo (rest (:todo parent))))]
    (-> stack pop (conj (process-frame (assoc child :resume resume))))))

(defn- resume-parent-frame [stack child-frame]
  (-> stack pop (conj (assoc (:resume child-frame) :ws (complete-child-ws child-frame)))))

(defn- process-conditional-step [form bindings _trace-ctx walk-state cctx]
  (when-let [branches (or (reducible-conditional-branches form bindings)
                          (conditional-branch-forms form))]
    (let [reducible? (boolean (reducible-conditional-branches form bindings))
          child-cctx (if reducible?
                       cctx
                       (let [{:keys [cause context]} (runtime-conditional-cause form)]
                         (push-cctx-cause cctx cause context)))]
      {:child {:todo branches
               :bindings bindings
               :ws (assoc walk-state :done-forms [])
               :cctx child-cctx
               :complete {:preceding :resume :seen-sut? :child}}
       :results []
       :preceding (:preceding walk-state)
       :seen-sut? (:seen-sut? walk-state)})))

(def ^:private max-flattened-dotimes 32)

(defn- flattenable-dotimes-count? [n-expr]
  (and (number? n-expr) (pos? n-expr) (<= n-expr max-flattened-dotimes)))

(defn- process-dotimes [form bindings trace-ctx cctx]
  (when (and (>= (count form) 3) (vector? (second form)))
    (let [[sym n-expr] (second form)
          body (drop 2 form)]
      (if (flattenable-dotimes-count? n-expr)
        (mapcat (fn [i]
                  (process-forms body
                                 (if (symbol? sym) (assoc bindings sym i) bindings)
                                 trace-ctx
                                 cctx))
                (range n-expr))
        (process-forms body bindings trace-ctx
                       (push-cctx-cause cctx :runtime-dotimes {:head 'dotimes :count n-expr}))))))

(defn- process-dotimes-step [form bindings trace-ctx walk-state cctx]
  (when (and (>= (count form) 3) (vector? (second form)))
    (let [[sym n-expr] (second form)
          flattened? (flattenable-dotimes-count? n-expr)]
      (if flattened?
        {:results (vec (process-dotimes form bindings trace-ctx cctx))
         :preceding (:preceding walk-state)
         :seen-sut? (:seen-sut? walk-state)}
        {:child {:todo (drop 2 form)
                 :bindings (if (symbol? sym) (assoc bindings sym 0) bindings)
                 :ws (assoc walk-state :done-forms [])
                 :cctx (push-cctx-cause cctx :runtime-dotimes {:head 'dotimes :count n-expr})
                 :complete {:preceding :resume :seen-sut? :child}}
         :results []
         :preceding (:preceding walk-state)
         :seen-sut? (:seen-sut? walk-state)}))))

(defn- process-doseq [form bindings trace-ctx cctx walk-state]
  (let [binding-form (second form)
        body (drop 2 form)
        done-forms (:done-forms walk-state [])]
    (if-not (and (vector? binding-form) (= 2 (count binding-form)))
      (process-forms body bindings trace-ctx
                     (push-cctx-cause cctx :malformed-doseq {:head 'doseq}))
      (let [[bind-expr coll-expr] binding-form
            body-cctx (doseq-conditional-cctx coll-expr bindings done-forms cctx)
            coll (doseq-coll coll-expr bindings)]
        (if (vector? coll)
          (mapcat (fn [item]
                    (process-forms body
                                   (bindings-for-doseq-item bind-expr item bindings)
                                   trace-ctx
                                   body-cctx))
                  coll)
          (process-forms body bindings trace-ctx body-cctx))))))

(defn- process-fn-invoke-step [form bindings _trace-ctx walk-state cctx]
  (when (and (seq? form) (symbol? (first form)) (seq (rest form)))
    (when-let [fn-form (get bindings (first form))]
      (when (fn-form? fn-form)
        (let [params (fn-param-syms fn-form)
              body (drop 2 fn-form)
              new-bindings (merge bindings (zipmap params (rest form)))]
          {:child {:todo body
                   :bindings new-bindings
                   :ws {:preceding nil :seen-sut? false :done-forms []}
                   :cctx cctx
                   :complete {:preceding :resume :seen-sut? :resume}}
           :results []
           :preceding (:preceding walk-state)
           :seen-sut? (:seen-sut? walk-state)})))))

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

(defn- ns-fn-bindings [forms]
  (into {}
        (keep (fn [form]
                (when-let [fn-lit (defn->fn-literal form)]
                  [(second form) fn-lit]))
              forms)))

(defn- helper-bindings-from-forms [forms]
  (into {}
        (keep (fn [form]
                (when-let [fn-lit (defn->fn-literal form)]
                  [(second form) fn-lit]))
              forms)))

(defn- test-line [form]
  (or (:line (meta form)) (:row (meta form))))

(defn- test-entry [form kind helper-bindings]
  {:form kind
   :test-name (second form)
   :body (drop 2 form)
   :line (test-line form)
   :helper-bindings helper-bindings})

(defn- find-tests-in-forms [forms inherited-helpers]
  (loop [pending (seq forms) results []]
    (if (empty? pending)
      results
      (let [form (first pending)]
        (cond
          (not (seq? form))
          (recur (rest pending) results)

          (#{'deftest 'it} (first form))
          (recur (rest pending)
                 (conj results (test-entry form
                                           (if (= 'deftest (first form)) :deftest :it)
                                           inherited-helpers)))

          (#{'describe 'context} (first form))
          (let [body (rest form)
                block-helpers (merge inherited-helpers (helper-bindings-from-forms body))]
            (recur (rest pending)
                   (into results (find-tests-in-forms body block-helpers))))

          :else
          (recur (rest pending) results))))))

(defn- find-tests [forms]
  (find-tests-in-forms (rest forms) (ns-fn-bindings forms)))

(defn- sut-invoke-form? [form _bindings trace-ctx]
  (trace/direct-sut-invoke-form? form trace-ctx))

(defn- form-reaches-sut? [form bindings trace-ctx]
  (or (sut-invoke-form? form bindings trace-ctx)
      (trace/reaches-sut-likely? form bindings trace-ctx)))

(def ^:private underlying-conditional-reasons
  {:extroverted :would-be-extroverted
   :likely-extroverted :would-be-likely-extroverted})

(defn- introverted-conditional-reason [reason]
  (or reason :no-sut-assertion))

(defn- conditional-assertion-reason [{:keys [verdict reason]}]
  (or (get underlying-conditional-reasons verdict)
      (and (= verdict :introverted) (introverted-conditional-reason reason))
      (and (= verdict :questionable) reason)
      :conditional-assertion))

(defn- as-conditional-assertion [result cctx]
  (let [{:keys [verdict reason trace]} result
        underlying-reason (conditional-assertion-reason result)
        inner (innermost-cctx-cause cctx)]
    {:verdict :conditional-assertion
     :reason underlying-reason
     :trace (cond-> (assoc trace
                           :conditional? true
                           :underlying-verdict verdict
                           :underlying-reason (or reason underlying-reason))
              inner (assoc :conditional-cause (:cause inner)
                           :conditional-context (:context inner))
              (seq (:causes cctx)) (assoc :conditional-causes (:causes cctx)))}))

(defn- finalize-assertion-result [result cctx]
  (if (and result (pos? (ctx-depth cctx)))
    (as-conditional-assertion result cctx)
    result))

(defn- assertion-result
  [form bindings trace-ctx cctx]
  (finalize-assertion-result
   (let [{:keys [verdict reason]} (trace/trace-form form bindings trace-ctx)]
     {:verdict verdict
      :reason reason
      :trace (trace/explain-trace form bindings trace-ctx)})
   cctx))

(defn- stub-assertion-result
  [assertion-form bindings trace-ctx preceding-sut cctx]
  (finalize-assertion-result
   (let [trace-target (or preceding-sut assertion-form)
         {:keys [verdict reason]} (trace/trace-form trace-target bindings trace-ctx)
         trace (assoc (trace/explain-trace trace-target bindings trace-ctx)
                      :assertion-form assertion-form
                      :preceding-sut-call preceding-sut)]
     {:verdict verdict :reason reason :trace trace})
   cctx))

(defn- questionable-result [form reason cctx]
  (finalize-assertion-result
   {:verdict :questionable
    :reason reason
    :trace {:assertion-form form}}
   cctx))

(defn- immediate-preceding-sut? [asserted-form bindings trace-ctx preceding]
  (and preceding
       (form-reaches-sut? preceding bindings trace-ctx)
       (trace/reaches-test-module? asserted-form bindings trace-ctx)))

(defn- atom-constructor-form? [form]
  (and (seq? form) (= 'atom (first form))))

(defn- atom-bound-sym? [bindings sym]
  (atom-constructor-form? (get bindings sym)))

(defn- deref-target-sym [form]
  (when (and (seq? form) (= 2 (count form)))
    (let [head (first form) arg (second form)]
      (when (#{'deref 'clojure.core/deref} head)
        (cond
          (symbol? arg) arg
          (and (seq? arg) (= 'var (first arg)) (symbol? (second arg))) (second arg)
          :else nil)))))

(defn- walk-children [node]
  (cond
    (map? node) (vals node)
    (vector? node) (seq node)
    (seq? node) (seq node)
    :else nil))

(defn- deref-form? [form]
  (and (seq? form) (= 2 (count form))
       (#{'deref 'clojure.core/deref} (first form))))

(defn- collect-deref-forms [form]
  (loop [stack [form] derefs []]
    (if (empty? stack)
      derefs
      (let [node (peek stack)
            stack (pop stack)]
        (if (deref-form? node)
          (recur (into stack (reverse (vec (or (walk-children node) []))))
                 (conj derefs node))
          (recur (into stack (reverse (vec (or (walk-children node) []))))
                 derefs))))))

(defn- deref-target-syms-in-form [form]
  (into #{} (keep deref-target-sym (collect-deref-forms form))))

(defn- symbols-in-form [form]
  (loop [stack [form] syms #{}]
    (if (empty? stack)
      syms
      (let [node (peek stack)
            stack (pop stack)
            syms (if (symbol? node) (conj syms node) syms)]
        (recur (into stack (reverse (vec (or (walk-children node) []))))
               syms)))))

(defn- let-bound-atom-syms [bindings]
  (into #{} (keep (fn [[k v]]
                    (when (and (symbol? k) (atom-constructor-form? v))
                      k))
                  bindings)))

(defn- symbols-in-form-outside-deref [form]
  (loop [stack [form] syms #{}]
    (if (empty? stack)
      syms
      (let [node (peek stack)
            stack (pop stack)
            syms (if (symbol? node) (conj syms node) syms)
            children (if (deref-form? node)
                       []
                       (or (walk-children node) []))]
        (recur (into stack (reverse (vec children))) syms)))))

(defn- sut-call-mutation-atoms [form bindings]
  (let [atom-syms (let-bound-atom-syms bindings)]
    (into (clojure.set/intersection atom-syms (symbols-in-form-outside-deref form))
          (mapcat (fn [sym]
                    (when (contains? bindings sym)
                      (clojure.set/intersection atom-syms
                                                (symbols-in-form (get bindings sym)))))
                  (symbols-in-form form)))))

(defn- atom-mutation-target-sym [form]
  (when (and (seq? form) (<= 2 (count form)))
    (let [head (first form) target (second form)]
      (when (#{'reset! 'swap! 'clojure.core/reset! 'clojure.core/swap!} head)
        (or (deref-target-sym target)
            (when (symbol? target) target))))))

(defn- atom-syms-written-in-form [form]
  (cond
    (nil? form) #{}
    (map? form) (into #{} (mapcat atom-syms-written-in-form (vals form)))
    (vector? form) (into #{} (mapcat atom-syms-written-in-form form))
    (seq? form)
    (into (or (when-let [sym (atom-mutation-target-sym form)] #{sym}) #{})
          (mapcat atom-syms-written-in-form form))
    :else #{}))

(defn- stub-capture-atoms-from-redefs [redefs-bindings]
  (when (vector? redefs-bindings)
    (reduce (fn [acc [_ stub-fn]]
              (if (fn-form? stub-fn)
                (into acc (atom-syms-written-in-form stub-fn))
                acc))
            #{}
            (partition 2 redefs-bindings))))

(defn- spy-atoms-written-in-form [form bindings]
  (set (filter #(atom-bound-sym? bindings %)
               (atom-syms-written-in-form form))))

(defn- advance-walk-state [form bindings trace-ctx walk-state]
  (let [sut? (form-reaches-sut? form bindings trace-ctx)
        spy-captures (when sut? (spy-atoms-written-in-form form bindings))
        mutation-atoms (when sut? (sut-call-mutation-atoms form bindings))]
    (cond-> (assoc walk-state :preceding form)
      sut? (assoc :seen-sut? true :last-sut-call form)
      (seq spy-captures)
      (update :stub-capture-atoms (fnil into #{}) spy-captures)
      (seq mutation-atoms)
      (update :sut-mutation-atoms (fnil into #{}) mutation-atoms))))

(defn- wiring-sut-call [walk-state bindings trace-ctx]
  (let [{:keys [last-sut-call preceding]} walk-state]
    (cond
      (and last-sut-call (form-reaches-sut? last-sut-call bindings trace-ctx))
      last-sut-call

      (and preceding (form-reaches-sut? preceding bindings trace-ctx))
      preceding

      :else nil)))

(defn- wiring-spy-atom-sym [asserted-form bindings walk-state]
  (some (fn [sym]
          (when (and (atom-bound-sym? bindings sym)
                     (contains? (:stub-capture-atoms walk-state) sym))
            sym))
        (deref-target-syms-in-form asserted-form)))

(defn- sut-atom-deref-target? [sym bindings trace-ctx]
  (and (symbol? sym)
       (not (contains? bindings sym))
       (not (atom-bound-sym? bindings sym))
       (trace/reaches-sut? (list 'clojure.core/deref sym) bindings trace-ctx)))

(defn- sut-atom-read-evidence [asserted-form bindings trace-ctx walk-state]
  (when (= :introverted (:verdict (trace/trace-form asserted-form bindings trace-ctx)))
    (when (and (:seen-sut? walk-state)
               (wiring-sut-call walk-state bindings trace-ctx))
      (when (some #(sut-atom-deref-target? % bindings trace-ctx)
                  (deref-target-syms-in-form asserted-form))
        :sut-atom-read))))

(defn- world-atom-readback-evidence [asserted-form bindings trace-ctx walk-state]
  (when (= :introverted (:verdict (trace/trace-form asserted-form bindings trace-ctx)))
    (when (and (:seen-sut? walk-state)
               (wiring-sut-call walk-state bindings trace-ctx)
               (seq (:sut-mutation-atoms walk-state)))
      (when (some #(and (atom-bound-sym? bindings %)
                        (contains? (:sut-mutation-atoms walk-state) %))
                  (deref-target-syms-in-form asserted-form))
        :world-atom-readback))))

(defn- wiring-capture-evidence [asserted-form bindings trace-ctx walk-state]
  (when (= :introverted (:verdict (trace/trace-form asserted-form bindings trace-ctx)))
    (when (and (:seen-sut? walk-state)
               (seq (:stub-capture-atoms walk-state))
               (wiring-sut-call walk-state bindings trace-ctx)
               (not (sut-atom-read-evidence asserted-form bindings trace-ctx walk-state)))
      (when (wiring-spy-atom-sym asserted-form bindings walk-state)
        :stub-capture))))

(defn- wiring-assertion-result
  [assertion-form asserted-form bindings trace-ctx walk-state cctx]
  (let [sut-call (wiring-sut-call walk-state bindings trace-ctx)]
    (finalize-assertion-result
     {:verdict :likely-extroverted
      :reason :sut-wiring-heuristic
      :trace (assoc (trace/explain-trace sut-call bindings trace-ctx)
                   :assertion-form assertion-form
                   :wiring-evidence :stub-capture
                   :preceding-sut-call sut-call)}
     cctx)))

(def ^:private file-interop-methods
  #{".exists" ".isDirectory" ".isFile" ".listFiles" ".startsWith" ".getPath" ".getName"})

(defn- interop-method-sym? [sym]
  (and (symbol? sym) (.startsWith (name sym) ".")))

(defn- files-invoke? [form]
  (and (seq? form) (symbol? (first form))
       (when-let [ns (namespace (first form))]
         (= "Files" (name ns)))))

(defn- file-dependency-form? [form]
  (cond
    (nil? form) false
    (and (seq? form) (= 'slurp (first form))) true
    (and (seq? form) (#{'File. 'java.io.File} (first form))) true
    (and (seq? form) (interop-method-sym? (first form))
         (contains? file-interop-methods (name (first form)))) true
    (files-invoke? form) true
    (seq? form) (boolean (some file-dependency-form? form))
    :else false))

(defn- file-dependency-evidence [asserted-form bindings trace-ctx walk-state]
  (when (= :introverted (:verdict (trace/trace-form asserted-form bindings trace-ctx)))
    (when (and (:seen-sut? walk-state)
               (wiring-sut-call walk-state bindings trace-ctx)
               (file-dependency-form? asserted-form))
      :file-dependency)))

(defn- file-dependency-assertion-result
  [assertion-form asserted-form bindings trace-ctx walk-state cctx]
  (let [sut-call (wiring-sut-call walk-state bindings trace-ctx)]
    (finalize-assertion-result
     {:verdict :likely-extroverted
      :reason :file-dependency
      :trace (assoc (trace/explain-trace sut-call bindings trace-ctx)
                   :assertion-form assertion-form
                   :external-dependency-evidence :file-dependency
                   :preceding-sut-call sut-call)}
     cctx)))

(defn- side-effect-evidence
  [asserted-form bindings trace-ctx walk-state]
  (when (= :introverted (:verdict (trace/trace-form asserted-form bindings trace-ctx)))
    (or (sut-atom-read-evidence asserted-form bindings trace-ctx walk-state)
        (world-atom-readback-evidence asserted-form bindings trace-ctx walk-state)
        (when (immediate-preceding-sut? asserted-form bindings trace-ctx (:preceding walk-state))
          :immediate-preceding-sut)
        (when (and (:seen-sut? walk-state)
                   (trace/binding-from-test-module? bindings trace-ctx))
          :test-state-binding))))

(defn- side-effect-assertion-result
  [assertion-form asserted-form bindings trace-ctx walk-state cctx]
  (let [evidence (side-effect-evidence asserted-form bindings trace-ctx walk-state)
        {:keys [preceding]} walk-state
        sut-call (wiring-sut-call walk-state bindings trace-ctx)
        trace-target (case evidence
                       (:sut-atom-read :world-atom-readback) sut-call
                       :immediate-preceding-sut preceding
                       :test-state-binding asserted-form
                       assertion-form)]
    (finalize-assertion-result
     {:verdict :likely-extroverted
      :reason :sut-side-effect-heuristic
      :trace (cond-> (trace/explain-trace trace-target bindings trace-ctx)
               true (assoc :assertion-form assertion-form
                           :side-effect-evidence evidence)
               (and sut-call (#{:sut-atom-read :world-atom-readback :immediate-preceding-sut} evidence))
               (assoc :preceding-sut-call sut-call))}
     cctx)))

(defn- build-finding-trace [trace-ctx sut assertion-results]
  {:test-ns (:test-ns trace-ctx)
   :requires (:requires trace-ctx)
   :refer-syms (:all-refer-syms trace-ctx)
   :sut-namespaces sut
   :assertions (vec (map :trace assertion-results))})

(defn- let-binding-pairs [form]
  (let [after-bindings (rest form)
        binding-form (first after-bindings)]
    {:pairs (if (vector? binding-form)
              (partition 2 binding-form)
              (partition 2 after-bindings))
     :body (rest after-bindings)}))

(defn- assoc-let-pair [bindings [k v]]
  (cond
    (symbol? k) (assoc bindings k v)
    (supported-destructure-pattern? k) (expand-destructure-bindings k v bindings)
    :else bindings))

(defn- let-bindings [form bindings]
  (let [{:keys [pairs body]} (let-binding-pairs form)
        unsupported? (some (fn [[k _]] (unsupported-destructure-pattern? k)) pairs)
        new-bindings (reduce assoc-let-pair bindings pairs)]
    {:body body
     :bindings (cond-> new-bindings unsupported? (assoc :destructuring? true))}))

(def ^:private walk-state-keys
  [:preceding :seen-sut? :stub-capture-atoms :last-sut-call :sut-mutation-atoms])

(defn- noop-step [walk-state]
  (select-keys walk-state walk-state-keys))

(defn- child-step [walk-state {:keys [todo bindings ws cctx complete]}]
  (assoc (noop-step walk-state)
         :child {:todo todo :bindings bindings :ws ws :cctx cctx :complete complete}
         :results []))

(defn- result-step [walk-state results]
  (assoc (noop-step walk-state) :results results))

(defn- fresh-walk-state [{:keys [seen-sut? stub-capture-atoms last-sut-call sut-mutation-atoms]}]
  {:preceding nil
   :seen-sut? seen-sut? :done-forms []
   :stub-capture-atoms stub-capture-atoms
   :last-sut-call last-sut-call
   :sut-mutation-atoms sut-mutation-atoms})

(defn- process-let-step [form bindings walk-state cctx]
  (let [{:keys [body bindings]} (let-bindings form bindings)
        {:keys [seen-sut?]} walk-state]
    (child-step walk-state {:todo body
                            :bindings bindings
                            :ws (fresh-walk-state walk-state)
                            :cctx cctx
                            :complete {:preceding :resume :seen-sut? :merge-or}})))

(defn- process-seq-child-step [form bindings walk-state cctx complete]
  (child-step walk-state {:todo (rest form)
                          :bindings bindings
                          :ws walk-state
                          :cctx cctx
                          :complete complete}))

(defn- process-with-redefs-step [form bindings walk-state cctx]
  (let [stub-captures (or (stub-capture-atoms-from-redefs (second form)) #{})]
    (child-step walk-state {:todo (drop 2 form)
                            :bindings bindings
                            :ws (assoc walk-state
                                       :done-forms []
                                       :stub-capture-atoms stub-captures)
                            :cctx cctx
                            :complete {:preceding :child :seen-sut? :child}})))

(defn- process-fn-step [form bindings walk-state cctx]
  (child-step walk-state {:todo (drop 2 form)
                          :bindings bindings
                          :ws (fresh-walk-state walk-state)
                          :cctx cctx
                          :complete {:preceding :resume :seen-sut? :merge-or}}))

(defn- process-parsed-assertion [form parsed bindings trace-ctx walk-state cctx]
  (let [{:keys [preceding]} walk-state]
    (cond
      (assertions/stub-invocation? parsed)
      (result-step walk-state [(stub-assertion-result form bindings trace-ctx preceding cctx)])

      (:reason parsed)
      (result-step walk-state [(questionable-result form (:reason parsed) cctx)])

      (wiring-capture-evidence (:asserted-form parsed) bindings trace-ctx walk-state)
      (let [advanced (advance-walk-state form bindings trace-ctx walk-state)]
        (assoc (select-keys advanced walk-state-keys)
               :results [(wiring-assertion-result form (:asserted-form parsed) bindings
                                                   trace-ctx walk-state cctx)]))

      (sut-atom-read-evidence (:asserted-form parsed) bindings trace-ctx walk-state)
      (let [advanced (advance-walk-state form bindings trace-ctx walk-state)]
        (assoc (select-keys advanced walk-state-keys)
               :results [(side-effect-assertion-result form (:asserted-form parsed) bindings
                                                        trace-ctx walk-state cctx)]))

      (file-dependency-evidence (:asserted-form parsed) bindings trace-ctx walk-state)
      (let [advanced (advance-walk-state form bindings trace-ctx walk-state)]
        (assoc (select-keys advanced walk-state-keys)
               :results [(file-dependency-assertion-result form (:asserted-form parsed) bindings
                                                           trace-ctx walk-state cctx)]))

      (side-effect-evidence (:asserted-form parsed) bindings trace-ctx walk-state)
      (let [advanced (advance-walk-state form bindings trace-ctx walk-state)]
        (assoc (select-keys advanced walk-state-keys)
               :results [(side-effect-assertion-result form (:asserted-form parsed) bindings
                                                        trace-ctx walk-state cctx)]))

      :else
      (let [advanced (advance-walk-state form bindings trace-ctx walk-state)]
        (assoc (select-keys advanced walk-state-keys)
               :results [(assertion-result (:asserted-form parsed) bindings trace-ctx cctx)])))))

(defn- process-default-step [form bindings trace-ctx walk-state cctx]
  (let [advanced (advance-walk-state form bindings trace-ctx walk-state)
        {:keys [seen-sut?]} walk-state]
    (if (form-reaches-sut? form bindings trace-ctx)
      (assoc (select-keys advanced walk-state-keys) :results [])
      (child-step advanced {:todo (rest form)
                            :bindings bindings
                            :ws (fresh-walk-state advanced)
                            :cctx cctx
                            :complete {:preceding (:preceding advanced) :seen-sut? :merge-or}}))))

(defn- process-expression-step [form bindings trace-ctx walk-state cctx]
  (or (process-conditional-step form bindings trace-ctx walk-state cctx)
      (process-fn-invoke-step form bindings trace-ctx walk-state cctx)
      (when-let [parsed (assertions/parse-assertion form)]
        (process-parsed-assertion form parsed bindings trace-ctx walk-state cctx))
      (process-default-step form bindings trace-ctx walk-state cctx)))

(def ^:private seq-child-complete {:preceding :child :seen-sut? :child})

(defn- process-seq-head [form bindings walk-state cctx]
  (process-seq-child-step form bindings walk-state cctx seq-child-complete))

(defn- process-dotimes-head [form bindings trace-ctx walk-state cctx]
  (or (process-dotimes-step form bindings trace-ctx walk-state cctx)
      (assoc (noop-step walk-state) :results [])))

(def ^:private head-form-steps
  {'let process-let-step
   'loop process-let-step
   'binding process-let-step
   'do process-seq-head
   'try process-seq-head
   'catch process-seq-head
   'finally process-seq-head
   'with-redefs process-with-redefs-step
   'fn process-fn-step})

(defn- head-form-step [head form bindings trace-ctx walk-state cctx]
  (cond
    (contains? head-form-steps head)
    ((get head-form-steps head) form bindings walk-state cctx)

    (= 'doseq head)
    (result-step walk-state (process-doseq form bindings trace-ctx cctx walk-state))

    (= 'dotimes head)
    (process-dotimes-head form bindings trace-ctx walk-state cctx)))

(defn- process-one-form [form bindings trace-ctx walk-state cctx]
  (if-not (seq? form)
    (assoc (noop-step walk-state) :results [])
    (or (head-form-step (first form) form bindings trace-ctx walk-state cctx)
        (process-expression-step form bindings trace-ctx walk-state cctx))))

(defn- initial-process-stack [forms bindings preceding seen-sut? cctx]
  [(process-frame {:todo forms
                   :bindings bindings
                   :ws {:preceding preceding
                        :seen-sut? seen-sut?
                        :done-forms []
                        :sut-mutation-atoms #{}}
                   :cctx cctx})])

(defn- finished-frame-result [frame results]
  (if (:resume frame)
    ::resume-parent
    {:results results
     :preceding (:preceding (:ws frame))
     :seen-sut? (:seen-sut? (:ws frame))}))

(defn- stack-after-step [stack frame step results]
  (if (:child step)
    [(push-child-frame stack (:child step)) (into results (:results step))]
    (let [processed (first (:todo frame))]
      [(conj (pop stack)
             (assoc frame
                    :todo (rest (:todo frame))
                    :ws (merge (:ws frame)
                               (select-keys step walk-state-keys)
                               {:done-forms (conj (:done-forms (:ws frame) []) processed)})))
       (into results (:results step))])))

(defn- process-forms-sequential
  [forms bindings trace-ctx preceding seen-sut? cctx]
   (loop [stack (initial-process-stack forms bindings preceding seen-sut? cctx)
          results []]
     (if (empty? stack)
       {:results results :preceding preceding :seen-sut? seen-sut?}
       (let [frame (peek stack)]
         (if (empty? (:todo frame))
           (let [outcome (finished-frame-result frame results)]
             (if (= ::resume-parent outcome)
               (recur (resume-parent-frame stack frame) results)
               outcome))
           (let [step (process-one-form (first (:todo frame))
                                        (:bindings frame)
                                        trace-ctx
                                        (:ws frame)
                                        (:cctx frame))
                 [stack' results'] (stack-after-step stack frame step results)]
             (recur stack' results')))))))

(defn- process-forms
  ([forms bindings trace-ctx]
   (process-forms forms bindings trace-ctx empty-cctx))
  ([forms bindings trace-ctx cctx]
   (:results (process-forms-sequential forms bindings trace-ctx nil false cctx))))

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
    {:verdict :likely-extroverted
     :reason (or (:reason match) :refer-all-heuristic)}))

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
           :let [results (vec (process-forms body helper-bindings trace-ctx))
                 {:keys [verdict reason]}
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
;; {:version 1, :tested-at "2026-06-21T11:15:39.42709-05:00", :module-hash "-397709833", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "-1458326996"} {:id "defn-/resolve-namespaced", :kind "defn-", :line 7, :end-line 11, :hash "1409516810"} {:id "defn-/resolve-unqualified", :kind "defn-", :line 13, :end-line 16, :hash "718817384"} {:id "defn-/resolve-ns-fn", :kind "defn-", :line 18, :end-line 23, :hash "-746974098"} {:id "defn-/destructuring-binding?", :kind "defn-", :line 25, :end-line 26, :hash "361096142"} {:id "form/5/declare", :kind "declare", :line 28, :end-line 28, :hash "-482327639"} {:id "defn-/vector-pattern-element?", :kind "defn-", :line 30, :end-line 31, :hash "-47316605"} {:id "defn-/vector-rest-index", :kind "defn-", :line 33, :end-line 34, :hash "-1649338098"} {:id "defn-/supported-vector-pattern?", :kind "defn-", :line 36, :end-line 43, :hash "-884333589"} {:id "def/map-destructure-keys", :kind "def", :line 45, :end-line 46, :hash "-599467106"} {:id "defn-/symbol-vector?", :kind "defn-", :line 48, :end-line 49, :hash "-1002205516"} {:id "defn-/standard-map-destructure-pattern?", :kind "defn-", :line 51, :end-line 56, :hash "-1501806290"} {:id "defn-/symbol-key-map-pattern?", :kind "defn-", :line 58, :end-line 59, :hash "1266033390"} {:id "defn-/supported-map-pattern?", :kind "defn-", :line 61, :end-line 63, :hash "328629814"} {:id "defn-/supported-destructure-pattern?", :kind "defn-", :line 65, :end-line 67, :hash "575065261"} {:id "defn-/unsupported-destructure-pattern?", :kind "defn-", :line 69, :end-line 71, :hash "820628602"} {:id "defn-/nth-binding", :kind "defn-", :line 73, :end-line 74, :hash "-1561782215"} {:id "defn-/drop-binding", :kind "defn-", :line 76, :end-line 77, :hash "-1024840710"} {:id "defn-/get-binding", :kind "defn-", :line 79, :end-line 80, :hash "120703912"} {:id "defn-/with-or-default", :kind "defn-", :line 82, :end-line 85, :hash "-1669426162"} {:id "form/20/declare", :kind "declare", :line 87, :end-line 87, :hash "176104591"} {:id "defn-/expand-vector-element", :kind "defn-", :line 89, :end-line 92, :hash "2078973933"} {:id "defn-/expand-vector-destructure", :kind "defn-", :line 94, :end-line 107, :hash "427439272"} {:id "defn-/map-lookup-key", :kind "defn-", :line 109, :end-line 110, :hash "223426931"} {:id "defn-/expand-symbol-key-map-destructure", :kind "defn-", :line 112, :end-line 117, :hash "1033180199"} {:id "defn-/bind-map-keys", :kind "defn-", :line 119, :end-line 124, :hash "1252663033"} {:id "defn-/bind-map-entry", :kind "defn-", :line 126, :end-line 132, :hash "55590744"} {:id "defn-/expand-map-destructure", :kind "defn-", :line 134, :end-line 140, :hash "2088334812"} {:id "defn-/expand-destructure-bindings", :kind "defn-", :line 142, :end-line 147, :hash "-866669811"} {:id "defn-/fn-form?", :kind "defn-", :line 149, :end-line 150, :hash "1534401844"} {:id "defn-/fn-param-syms", :kind "defn-", :line 152, :end-line 154, :hash "-1934413924"} {:id "defn-/doseq-coll", :kind "defn-", :line 156, :end-line 160, :hash "990849186"} {:id "form/32/declare", :kind "declare", :line 162, :end-line 162, :hash "-97545983"} {:id "defn-/bindings-for-doseq-item", :kind "defn-", :line 164, :end-line 172, :hash "54363040"} {:id "defn-/literal-true?", :kind "defn-", :line 174, :end-line 174, :hash "-1333819690"} {:id "defn-/literal-false?", :kind "defn-", :line 175, :end-line 175, :hash "-1313096779"} {:id "defn-/assertion-form?", :kind "defn-", :line 177, :end-line 178, :hash "238628140"} {:id "form/37/declare", :kind "declare", :line 180, :end-line 180, :hash "-1596160099"} {:id "defn-/nested-forms-contain-assertion?", :kind "defn-", :line 182, :end-line 183, :hash "-28890695"} {:id "defn-/do-form-contains-assertion?", :kind "defn-", :line 185, :end-line 186, :hash "-947315160"} {:id "defn-/binding-form-contains-assertion?", :kind "defn-", :line 188, :end-line 190, :hash "-1545568470"} {:id "defn-/contains-assertion?", :kind "defn-", :line 192, :end-line 198, :hash "2063853848"} {:id "def/non-empty-coll-heads", :kind "def", :line 200, :end-line 200, :hash "-134423271"} {:id "defn-/non-empty-coll-expr", :kind "defn-", :line 202, :end-line 206, :hash "-1627812733"} {:id "defn-/seq-not-empty-guard?", :kind "defn-", :line 208, :end-line 212, :hash "1190904328"} {:id "defn-/empty?-guard?", :kind "defn-", :line 214, :end-line 218, :hash "226372305"} {:id "defn-/should-be-nil-guard?", :kind "defn-", :line 220, :end-line 221, :hash "654439219"} {:id "defn-/non-empty-guard-assertion?", :kind "defn-", :line 223, :end-line 230, :hash "-981564608"} {:id "defn-/preceded-by-non-empty-guard?", :kind "defn-", :line 232, :end-line 236, :hash "857877616"} {:id "defn-/flattenable-doseq-coll?", :kind "defn-", :line 238, :end-line 239, :hash "-1596735075"} {:id "def/empty-cctx", :kind "def", :line 241, :end-line 241, :hash "-1384542187"} {:id "defn-/ctx-depth", :kind "defn-", :line 243, :end-line 243, :hash "-986116504"} {:id "defn-/push-cctx-cause", :kind "defn-", :line 245, :end-line 247, :hash "1446676840"} {:id "defn-/innermost-cctx-cause", :kind "defn-", :line 249, :end-line 251, :hash "989259605"} {:id "defn-/symbol-in-form?", :kind "defn-", :line 253, :end-line 257, :hash "708742097"} {:id "defn-/near-doseq-guard?", :kind "defn-", :line 259, :end-line 266, :hash "1599044081"} {:id "defn-/doseq-conditional-cctx", :kind "defn-", :line 268, :end-line 282, :hash "-193712406"} {:id "defn-/partial-dispatch-if?", :kind "defn-", :line 284, :end-line 289, :hash "358443859"} {:id "defn-/runtime-conditional-cause", :kind "defn-", :line 291, :end-line 294, :hash "-1119272564"} {:id "defn-/reducible-cond-step", :kind "defn-", :line 296, :end-line 301, :hash "139656575"} {:id "defn-/reducible-cond-iterate", :kind "defn-", :line 303, :end-line 308, :hash "-1970172388"} {:id "defn-/reducible-cond-branches", :kind "defn-", :line 310, :end-line 318, :hash "-1238061652"} {:id "defn-/reducible-unary-conditional-branches", :kind "defn-", :line 320, :end-line 327, :hash "-596630077"} {:id "defn-/reducible-if-literal-branches", :kind "defn-", :line 329, :end-line 335, :hash "-1652451699"} {:id "def/not-literal-sentinel", :kind "def", :line 337, :end-line 337, :hash "2120817007"} {:id "defn-/literal-case-dispatch-value?", :kind "defn-", :line 339, :end-line 340, :hash "1032365407"} {:id "defn-/resolve-literal-value", :kind "defn-", :line 342, :end-line 355, :hash "-1176256840"} {:id "defn-/case-matching-expr", :kind "defn-", :line 357, :end-line 365, :hash "353267885"} {:id "defn-/reducible-case-literal-branches", :kind "defn-", :line 367, :end-line 372, :hash "-1298947904"} {:id "defn-/reducible-literal-branches", :kind "defn-", :line 374, :end-line 386, :hash "893460775"} {:id "defn-/dispatch-if-branches", :kind "defn-", :line 388, :end-line 392, :hash "1862463126"} {:id "defn-/reducible-conditional-branches", :kind "defn-", :line 394, :end-line 397, :hash "2032608910"} {:id "def/conditional-head-syms", :kind "def", :line 399, :end-line 401, :hash "-2029598174"} {:id "defn-/cond-branch-forms", :kind "defn-", :line 403, :end-line 410, :hash "724006868"} {:id "defn-/case-branch-forms", :kind "defn-", :line 412, :end-line 419, :hash "1041144653"} {:id "defn-/condp-branch-forms", :kind "defn-", :line 421, :end-line 422, :hash "-292336679"} {:id "defn-/if-branch-forms", :kind "defn-", :line 424, :end-line 426, :hash "-546724505"} {:id "def/conditional-branch-extractors", :kind "def", :line 428, :end-line 442, :hash "-98756298"} {:id "defn-/conditional-branch-forms", :kind "defn-", :line 444, :end-line 447, :hash "1582157075"} {:id "form/79/declare", :kind "declare", :line 449, :end-line 449, :hash "990345103"} {:id "defn-/process-frame", :kind "defn-", :line 451, :end-line 453, :hash "-1521784206"} {:id "defn-/complete-preceding", :kind "defn-", :line 455, :end-line 459, :hash "378913017"} {:id "defn-/complete-seen-sut?", :kind "defn-", :line 461, :end-line 466, :hash "1494113547"} {:id "defn-/complete-child-ws", :kind "defn-", :line 468, :end-line 470, :hash "333210935"} {:id "defn-/push-child-frame", :kind "defn-", :line 472, :end-line 475, :hash "687311338"} {:id "defn-/resume-parent-frame", :kind "defn-", :line 477, :end-line 478, :hash "873158305"} {:id "defn-/process-conditional-step", :kind "defn-", :line 480, :end-line 495, :hash "1772954480"} {:id "def/max-flattened-dotimes", :kind "def", :line 497, :end-line 497, :hash "-2145562049"} {:id "defn-/flattenable-dotimes-count?", :kind "defn-", :line 499, :end-line 500, :hash "1558346506"} {:id "defn-/process-dotimes", :kind "defn-", :line 502, :end-line 514, :hash "821701247"} {:id "defn-/process-dotimes-step", :kind "defn-", :line 516, :end-line 531, :hash "1629484075"} {:id "defn-/process-doseq", :kind "defn-", :line 533, :end-line 550, :hash "1372762874"} {:id "defn-/process-fn-invoke-step", :kind "defn-", :line 552, :end-line 566, :hash "1963414532"} {:id "defn-/defn-arity-fn-literal", :kind "defn-", :line 568, :end-line 569, :hash "7584441"} {:id "defn-/defn-docstring-fn-literal", :kind "defn-", :line 571, :end-line 572, :hash "-396941995"} {:id "defn-/defn-docstring?", :kind "defn-", :line 574, :end-line 575, :hash "1220406710"} {:id "defn-/defn->fn-literal", :kind "defn-", :line 577, :end-line 583, :hash "-397160554"} {:id "defn-/ns-fn-bindings", :kind "defn-", :line 585, :end-line 590, :hash "656755951"} {:id "defn-/helper-bindings-from-forms", :kind "defn-", :line 592, :end-line 597, :hash "-1671807993"} {:id "defn-/test-line", :kind "defn-", :line 599, :end-line 600, :hash "-1424048643"} {:id "defn-/test-entry", :kind "defn-", :line 602, :end-line 607, :hash "-1355136209"} {:id "defn-/find-tests-in-forms", :kind "defn-", :line 609, :end-line 631, :hash "-1342048223"} {:id "defn-/find-tests", :kind "defn-", :line 633, :end-line 634, :hash "-278106521"} {:id "defn-/sut-invoke-form?", :kind "defn-", :line 636, :end-line 637, :hash "966618037"} {:id "defn-/form-reaches-sut?", :kind "defn-", :line 639, :end-line 641, :hash "30025062"} {:id "defn-/advance-walk-state", :kind "defn-", :line 643, :end-line 646, :hash "1927976431"} {:id "def/underlying-conditional-reasons", :kind "def", :line 648, :end-line 650, :hash "1473226787"} {:id "defn-/introverted-conditional-reason", :kind "defn-", :line 652, :end-line 653, :hash "2062598749"} {:id "defn-/conditional-assertion-reason", :kind "defn-", :line 655, :end-line 659, :hash "1948739796"} {:id "defn-/as-conditional-assertion", :kind "defn-", :line 661, :end-line 673, :hash "1835124783"} {:id "defn-/finalize-assertion-result", :kind "defn-", :line 675, :end-line 678, :hash "1962779200"} {:id "defn-/assertion-result", :kind "defn-", :line 680, :end-line 687, :hash "545324977"} {:id "defn-/stub-assertion-result", :kind "defn-", :line 689, :end-line 698, :hash "452423001"} {:id "defn-/questionable-result", :kind "defn-", :line 700, :end-line 705, :hash "805021069"} {:id "defn-/immediate-preceding-sut?", :kind "defn-", :line 707, :end-line 710, :hash "535255210"} {:id "defn-/atom-constructor-form?", :kind "defn-", :line 712, :end-line 713, :hash "-422734210"} {:id "defn-/atom-bound-sym?", :kind "defn-", :line 715, :end-line 716, :hash "-760657684"} {:id "defn-/deref-target-sym", :kind "defn-", :line 718, :end-line 722, :hash "-930369276"} {:id "defn-/atom-mutation-target-sym", :kind "defn-", :line 724, :end-line 729, :hash "64866046"} {:id "defn-/atom-syms-written-in-form", :kind "defn-", :line 731, :end-line 738, :hash "-508306755"} {:id "defn-/stub-capture-atoms-from-redefs", :kind "defn-", :line 740, :end-line 747, :hash "166231913"} {:id "defn-/wiring-sut-call", :kind "defn-", :line 749, :end-line 758, :hash "-804486180"} {:id "defn-/wiring-capture-evidence", :kind "defn-", :line 760, :end-line 768, :hash "-1708091701"} {:id "defn-/wiring-assertion-result", :kind "defn-", :line 770, :end-line 780, :hash "1248404013"} {:id "defn-/side-effect-evidence", :kind "defn-", :line 782, :end-line 788, :hash "-1255916380"} {:id "defn-/side-effect-assertion-result", :kind "defn-", :line 790, :end-line 806, :hash "-1977496066"} {:id "defn-/build-finding-trace", :kind "defn-", :line 808, :end-line 813, :hash "377525482"} {:id "defn-/let-binding-pairs", :kind "defn-", :line 815, :end-line 821, :hash "-1105506548"} {:id "defn-/assoc-let-pair", :kind "defn-", :line 823, :end-line 827, :hash "699566064"} {:id "defn-/let-bindings", :kind "defn-", :line 829, :end-line 834, :hash "-161395392"} {:id "def/walk-state-keys", :kind "def", :line 836, :end-line 837, :hash "-7465522"} {:id "defn-/noop-step", :kind "defn-", :line 839, :end-line 840, :hash "49658498"} {:id "defn-/child-step", :kind "defn-", :line 842, :end-line 845, :hash "-942969747"} {:id "defn-/result-step", :kind "defn-", :line 847, :end-line 848, :hash "1574422238"} {:id "defn-/fresh-walk-state", :kind "defn-", :line 850, :end-line 854, :hash "-98138898"} {:id "defn-/process-let-step", :kind "defn-", :line 856, :end-line 863, :hash "-918336671"} {:id "defn-/process-seq-child-step", :kind "defn-", :line 865, :end-line 870, :hash "-814113114"} {:id "defn-/process-with-redefs-step", :kind "defn-", :line 872, :end-line 880, :hash "1922402911"} {:id "defn-/process-fn-step", :kind "defn-", :line 882, :end-line 887, :hash "741284536"} {:id "defn-/process-parsed-assertion", :kind "defn-", :line 889, :end-line 913, :hash "-1228297432"} {:id "defn-/process-default-step", :kind "defn-", :line 915, :end-line 924, :hash "1576563894"} {:id "defn-/process-expression-step", :kind "defn-", :line 926, :end-line 931, :hash "695210630"} {:id "def/seq-child-complete", :kind "def", :line 933, :end-line 933, :hash "-1727835598"} {:id "defn-/process-seq-head", :kind "defn-", :line 935, :end-line 936, :hash "-110688850"} {:id "defn-/process-dotimes-head", :kind "defn-", :line 938, :end-line 940, :hash "-637715473"} {:id "def/head-form-steps", :kind "def", :line 942, :end-line 950, :hash "302488026"} {:id "defn-/head-form-step", :kind "defn-", :line 952, :end-line 961, :hash "-1805949168"} {:id "defn-/process-one-form", :kind "defn-", :line 963, :end-line 967, :hash "-2020723366"} {:id "defn-/initial-process-stack", :kind "defn-", :line 969, :end-line 973, :hash "980868665"} {:id "defn-/finished-frame-result", :kind "defn-", :line 975, :end-line 980, :hash "1782626509"} {:id "defn-/stack-after-step", :kind "defn-", :line 982, :end-line 992, :hash "551500042"} {:id "defn-/process-forms-sequential", :kind "defn-", :line 994, :end-line 1012, :hash "202677959"} {:id "defn-/process-forms", :kind "defn-", :line 1014, :end-line 1018, :hash "1263996263"} {:id "defn-/body-form", :kind "defn-", :line 1020, :end-line 1023, :hash "576180413"} {:id "defn-/promote-cloistered", :kind "defn-", :line 1025, :end-line 1029, :hash "1925045456"} {:id "defn-/unconditional-assertion-results", :kind "defn-", :line 1031, :end-line 1032, :hash "-1579530133"} {:id "defn-/strongest-conditional-reason", :kind "defn-", :line 1034, :end-line 1040, :hash "-1458929829"} {:id "defn-/finding-conditional-cause", :kind "defn-", :line 1042, :end-line 1046, :hash "-1528230258"} {:id "defn-/finding-conditional-context", :kind "defn-", :line 1048, :end-line 1054, :hash "-1256792333"} {:id "defn-/first-unconditional-verdict", :kind "defn-", :line 1056, :end-line 1057, :hash "-2124349936"} {:id "defn-/extroverted-verdict", :kind "defn-", :line 1059, :end-line 1061, :hash "-1093477023"} {:id "defn-/likely-extroverted-verdict", :kind "defn-", :line 1063, :end-line 1066, :hash "-501801606"} {:id "defn-/questionable-verdict", :kind "defn-", :line 1068, :end-line 1070, :hash "566982830"} {:id "defn-/introverted-verdict", :kind "defn-", :line 1072, :end-line 1074, :hash "1774438313"} {:id "defn-/unconditional-verdict", :kind "defn-", :line 1076, :end-line 1080, :hash "1145335451"} {:id "defn-/test-verdict", :kind "defn-", :line 1082, :end-line 1087, :hash "-1307164133"} {:id "defn-/findings-for-forms", :kind "defn-", :line 1089, :end-line 1120, :hash "-503663862"} {:id "defn/analyze-forms", :kind "defn", :line 1122, :end-line 1125, :hash "-169972165"} {:id "defn/analyze-file", :kind "defn", :line 1127, :end-line 1131, :hash "1315649154"}]}
;; clj-mutate-manifest-end
