(ns deintroverter.walk
  (:require [deintroverter.assertions :as assertions]
            [deintroverter.forms :as forms]
            [deintroverter.heuristics.external :as external]
            [deintroverter.provenance :as prov]
            [deintroverter.reasons :as reasons]
            [deintroverter.trace :as trace]))

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

(declare process-forms process-forms-results)

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

(defn- gen-sample-head? [head]
  (or (= 'gen/sample head)
      (= 'clojure.spec.gen.alpha/sample head)))

(defn- gen-sample-size-trusted? [form]
  (or (= 2 (count form))
      (and (= 3 (count form))
           (let [size (nth form 2)]
             (and (number? size) (pos? size))))))

(defn- gen-sample-form? [form]
  (and (seq? form)
       (>= (count form) 2)
       (gen-sample-head? (first form))
       (gen-sample-size-trusted? form)))

(defn- trusted-generative-doseq-coll? [coll-expr]
  (gen-sample-form? coll-expr))

(def ^:private empty-cctx {:depth 0 :causes []})

(defn- ctx-depth [cctx] (:depth cctx 0))

(defn- push-cctx-cause [cctx cause context]
  {:depth (inc (ctx-depth cctx))
   :causes (conj (:causes cctx []) {:cause cause :context context})})

(defn- innermost-cctx-cause [cctx]
  (when (pos? (ctx-depth cctx))
    (last (:causes cctx))))

(defn- near-doseq-guard? [done-forms coll-expr]
  (when (symbol? coll-expr)
    (boolean
     (some (fn [form]
             (and (assertion-form? form)
                  (not (non-empty-guard-assertion? form coll-expr))
                  (forms/symbol-in-form? coll-expr form)))
           (take-last 2 done-forms)))))

(defn- doseq-guarded-cctx? [coll-expr bindings done-forms]
  (or (flattenable-doseq-coll? coll-expr bindings)
      (preceded-by-non-empty-guard? done-forms coll-expr)
      (trusted-generative-doseq-coll? coll-expr)))

(defn- doseq-symbol-cctx [coll-expr done-forms cctx]
  (push-cctx-cause cctx
                   (if (near-doseq-guard? done-forms coll-expr)
                     :near-doseq-guard
                     :missing-doseq-guard)
                   {:head 'doseq :coll coll-expr}))

(defn- doseq-conditional-cctx [coll-expr bindings done-forms cctx]
  (cond
    (doseq-guarded-cctx? coll-expr bindings done-forms) cctx
    (symbol? coll-expr) (doseq-symbol-cctx coll-expr done-forms cctx)
    :else (push-cctx-cause cctx :non-flattenable-doseq {:head 'doseq :coll coll-expr})))

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

(defn- binding-literal-value [expr bindings]
  (when (and (symbol? expr) (contains? bindings expr))
    (get bindings expr)))

(defn- resolve-literal-value [expr bindings]
  (let [value (or (binding-literal-value expr bindings)
                  (when (literal-case-dispatch-value? expr) expr)
                  not-literal-sentinel)]
    (if (literal-case-dispatch-value? value) value not-literal-sentinel)))

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
  (let [parent-ws (:ws (:resume child-frame))
        child-ws (:ws child-frame)]
    {:preceding (complete-preceding child-frame)
     :seen-sut? (complete-seen-sut? child-frame)
     :stub-capture-atoms (into (or (:stub-capture-atoms parent-ws) #{})
                               (or (:stub-capture-atoms child-ws) #{}))
     :sut-mutation-atoms (into (or (:sut-mutation-atoms parent-ws) #{})
                               (or (:sut-mutation-atoms child-ws) #{}))
     :last-sut-call (or (:last-sut-call child-ws)
                        (:last-sut-call parent-ws))}))

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
                  (process-forms-results body
                                         (if (symbol? sym) (assoc bindings sym i) bindings)
                                         trace-ctx
                                         cctx))
                (range n-expr))
        (process-forms-results body bindings trace-ctx
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
      (process-forms-results body bindings trace-ctx
                             (push-cctx-cause cctx :malformed-doseq {:head 'doseq}))
      (let [[bind-expr coll-expr] binding-form
            body-cctx (doseq-conditional-cctx coll-expr bindings done-forms cctx)
            coll (doseq-coll coll-expr bindings)]
        (if (vector? coll)
          (mapcat (fn [item]
                    (process-forms-results body
                                           (bindings-for-doseq-item bind-expr item bindings)
                                           trace-ctx
                                           body-cctx))
                  coll)
          (process-forms-results body bindings trace-ctx body-cctx))))))

(defn- invoke-arg-value [arg bindings]
  (if (and (symbol? arg) (contains? bindings arg))
    (get bindings arg)
    arg))

(defn- process-fn-invoke-step [form bindings _trace-ctx walk-state cctx]
  (when (and (seq? form) (symbol? (first form)))
    (when-let [fn-form (get bindings (first form))]
      (when (fn-form? fn-form)
        (let [params (fn-param-syms fn-form)
              body (drop 2 fn-form)
              new-bindings (merge bindings
                                  (zipmap params
                                          (map #(invoke-arg-value % bindings) (rest form))))]
          {:child {:todo body
                   :bindings new-bindings
                   :ws {:preceding nil
                        :seen-sut? (:seen-sut? walk-state)
                        :done-forms []
                        :stub-capture-atoms (:stub-capture-atoms walk-state)
                        :last-sut-call (:last-sut-call walk-state)
                        :sut-mutation-atoms (:sut-mutation-atoms walk-state)}
                   :cctx cctx
                   :complete {:preceding :resume :seen-sut? :merge-or}}
           :results []
           :preceding (:preceding walk-state)
           :seen-sut? (:seen-sut? walk-state)})))))

(defn- sut-invoke-form? [form _bindings trace-ctx]
  (trace/direct-sut-invoke-form? form trace-ctx))

(defn- bound-form-reaches-sut? [form bindings trace-ctx]
  (when (and (seq? form) (symbol? (first form)))
    (when-let [bound (get bindings (first form))]
      (or (trace/reaches-sut-likely? bound bindings trace-ctx)
          (= :sut-invoke (:kind (prov/derive-provenance bound bindings trace-ctx)))))))

(defn- form-reaches-sut? [form bindings trace-ctx]
  (or (sut-invoke-form? form bindings trace-ctx)
      (trace/reaches-sut-likely? form bindings trace-ctx)
      (= :sut-invoke (:kind (prov/derive-provenance form bindings trace-ctx)))
      (bound-form-reaches-sut? form bindings trace-ctx)))

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

(def ^:private non-asserted-subject-heads
  #{'deref 'clojure.core/deref})

(defn- production-invoke-form? [form bindings trace-ctx]
  (and (seq? form) (symbol? (first form))
       (not (contains? non-asserted-subject-heads (first form)))
       (or (form-reaches-sut? form bindings trace-ctx)
           (trace/namespaced-production-invoke? form bindings trace-ctx))))

(defn- asserted-production-invoke? [form bindings trace-ctx]
  (let [subject (trace/resolve-bound-form form bindings)]
    (boolean (some #(production-invoke-form? % bindings trace-ctx)
                   (forms/collect-seq-forms subject)))))

(defn- direct-assertion-evidence [subject bindings trace-ctx]
  (if (production-invoke-form? subject bindings trace-ctx)
    :sut-invoke
    :nested-sut-invoke))

(defn- direct-assertion-result [form bindings trace-ctx cctx]
  (let [subject (trace/resolve-bound-form form bindings)
        sut? (form-reaches-sut? subject bindings trace-ctx)
        trace-target (or (some #(when (production-invoke-form? % bindings trace-ctx) %)
                               (forms/collect-seq-forms subject))
                         subject)]
    (finalize-assertion-result
     {:verdict (if sut? :extroverted :likely-extroverted)
      :reason :provenance-link
      :reason-legacy :sut-direct-assertion-heuristic
      :trace (assoc (trace/explain-trace trace-target bindings trace-ctx)
                    :assertion-form form
                    :direct-assertion-evidence (direct-assertion-evidence subject bindings trace-ctx)
                    :link-via (if sut? :sut-invoke :sut-derived)
                    :prov-source trace-target)}
     cctx)))

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

(defn- advance-walk-state [form bindings trace-ctx walk-state]
  (let [sut? (form-reaches-sut? form bindings trace-ctx)
        spy-captures (when sut? (external/spy-atoms-written-in-form form bindings))
        mutation-atoms (when sut? (external/sut-call-mutation-atoms form bindings))]
    (cond-> (assoc walk-state :preceding form)
      sut? (assoc :seen-sut? true :last-sut-call form)
      (seq spy-captures)
      (update :stub-capture-atoms (fnil into #{}) spy-captures)
      (seq mutation-atoms)
      (update :sut-mutation-atoms (fnil into #{}) mutation-atoms))))

(defn- provenance-trace-target [assertion-form link walk-state bindings trace-ctx]
  (let [evidence (:legacy-evidence link)
        sut-call (external/wiring-sut-call walk-state bindings trace-ctx)]
    (case evidence
      :sut-result-read (:source link)
      :exception-catch-assertion (or sut-call assertion-form)
      assertion-form)))

(defn- provenance-side-effect-result
  [assertion-form asserted-form bindings trace-ctx walk-state cctx link]
  (let [evidence (:legacy-evidence link)
        sut-call (external/wiring-sut-call walk-state bindings trace-ctx)
        trace-target (provenance-trace-target assertion-form link walk-state bindings trace-ctx)]
    (finalize-assertion-result
     {:verdict :likely-extroverted
      :reason :provenance-link
      :reason-legacy :sut-side-effect-heuristic
      :trace (cond-> (trace/explain-trace trace-target bindings trace-ctx)
               true (assoc :assertion-form assertion-form
                           :side-effect-evidence evidence
                           :link-via (:kind link)
                           :prov-source (:source link))
               (or sut-call (= :sut-result-read evidence))
               (assoc :preceding-sut-call (or sut-call (:source link))))}
     cctx)))

(defn- provenance-link-opts [walk-state]
  {:walk-state walk-state :require-seen-sut? true})

(defn- introverted-trace-result [form bindings trace-ctx cctx]
  (let [{:keys [reason]} (trace/trace-form form bindings trace-ctx)]
    (finalize-assertion-result
     {:verdict :introverted
      :reason reason
      :trace (trace/explain-trace form bindings trace-ctx)}
     cctx)))

(defn- process-assertion-verdict
  [form asserted bindings trace-ctx walk-state cctx step]
  (let [{:keys [verdict reason]} (trace/trace-form asserted bindings trace-ctx)
        prov-opts (provenance-link-opts walk-state)
        prov-link (when (= :introverted verdict)
                    (prov/provenance-link? asserted bindings trace-ctx prov-opts))
        result (cond
                 (not= :introverted verdict)
                 (finalize-assertion-result
                  {:verdict verdict
                   :reason reason
                   :trace (trace/explain-trace asserted bindings trace-ctx)}
                  cctx)

                 prov-link
                 (provenance-side-effect-result
                  form asserted bindings trace-ctx walk-state cctx prov-link)

                 (asserted-production-invoke? asserted bindings trace-ctx)
                 (direct-assertion-result asserted bindings trace-ctx cctx)

                 :else
                 (introverted-trace-result asserted bindings trace-ctx cctx))]
    (assoc step :results [result])))

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
  (let [stub-captures (or (external/stub-capture-atoms-from-redefs (second form)) #{})]
    (child-step walk-state {:todo (drop 2 form)
                            :bindings bindings
                            :ws (assoc walk-state
                                       :done-forms []
                                       :stub-capture-atoms stub-captures)
                            :cctx cctx
                            :complete {:preceding :child :seen-sut? :child}})))

(defn- process-catch-step [form bindings walk-state cctx]
  (when (and (seq? form) (= 'catch (first form)) (>= (count form) 4) (symbol? (nth form 2)))
    (child-step walk-state {:todo (drop 3 form)
                            :bindings (assoc bindings (nth form 2) prov/catch-exception-marker)
                            :ws walk-state
                            :cctx cctx
                            :complete {:preceding :child :seen-sut? :child}})))

(defn- process-fn-step [form bindings walk-state cctx]
  (child-step walk-state {:todo (drop 2 form)
                          :bindings bindings
                          :ws (fresh-walk-state walk-state)
                          :cctx cctx
                          :complete {:preceding :resume :seen-sut? :merge-or}}))

(defn- external-assertion-step
  [form bindings trace-ctx walk-state cctx step asserted]
  (when-let [evidence (external/external-evidence asserted bindings trace-ctx walk-state)]
    (assoc step :results [(finalize-assertion-result
                           (external/external-assertion-result-map
                            form asserted bindings trace-ctx walk-state evidence)
                           cctx)])))

(defn- process-parsed-assertion [form parsed bindings trace-ctx walk-state cctx]
  (let [{:keys [preceding]} walk-state
        asserted (:asserted-form parsed)
        advanced (advance-walk-state form bindings trace-ctx walk-state)
        step (select-keys advanced walk-state-keys)]
    (cond
      (assertions/stub-invocation? parsed)
      (result-step walk-state [(stub-assertion-result form bindings trace-ctx preceding cctx)])

      (:reason parsed)
      (result-step walk-state [(questionable-result form (:reason parsed) cctx)])

      :else
      (or (external-assertion-step form bindings trace-ctx walk-state cctx step asserted)
          (process-assertion-verdict form asserted bindings trace-ctx walk-state cctx step)))))

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
   'catch process-catch-step
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

(defn- process-forms-results
  ([forms bindings trace-ctx]
   (process-forms-results forms bindings trace-ctx empty-cctx))
  ([forms bindings trace-ctx cctx]
   (:results (process-forms forms bindings trace-ctx {:cctx cctx}))))

(defn process-forms
  "Walk test-body forms and return assertion results.
  opts may include :cctx for conditional context."
  ([forms bindings trace-ctx]
   (process-forms forms bindings trace-ctx {}))
  ([forms bindings trace-ctx opts]
   (let [cctx (or (:cctx opts) empty-cctx)
         {:keys [results preceding seen-sut?]}
         (process-forms-sequential forms bindings trace-ctx nil false cctx)]
     {:results results
      :walk-state {:preceding preceding :seen-sut? seen-sut?}})))
