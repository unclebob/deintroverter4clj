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
  [{:keys [todo bindings ws cd resume complete]}]
  {:todo (seq todo) :bindings bindings :ws ws :cd cd :resume resume :complete complete})

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

(defn- process-conditional-step [form bindings _trace-ctx walk-state conditional-depth]
  (when-let [branches (conditional-branch-forms form)]
    {:child {:todo branches
             :bindings bindings
             :ws walk-state
             :cd (inc conditional-depth)
             :complete {:preceding :resume :seen-sut? :child}}
     :results []
     :preceding (:preceding walk-state)
     :seen-sut? (:seen-sut? walk-state)}))

(defn- process-dotimes-step [form bindings _trace-ctx walk-state conditional-depth]
  (when (and (>= (count form) 3) (vector? (second form)))
    (let [[sym _n] (second form)
          body (drop 2 form)
          bindings (if (symbol? sym) (assoc bindings sym 0) bindings)]
      {:child {:todo body
               :bindings bindings
               :ws walk-state
               :cd (inc conditional-depth)
               :complete {:preceding :resume :seen-sut? :child}}
       :results []
       :preceding (:preceding walk-state)
       :seen-sut? (:seen-sut? walk-state)})))

(defn- process-doseq [form bindings trace-ctx conditional-depth]
  (let [binding-form (second form)
        body (drop 2 form)
        body-depth (inc conditional-depth)]
    (if-not (and (vector? binding-form) (= 2 (count binding-form)))
      (process-forms body bindings trace-ctx body-depth)
      (let [[bind-expr coll-expr] binding-form
            coll (doseq-coll coll-expr bindings)]
        (if (vector? coll)
          (mapcat (fn [item]
                    (process-forms body
                                   (bindings-for-doseq-item bind-expr item bindings)
                                   trace-ctx
                                   body-depth))
                  coll)
          (process-forms body bindings trace-ctx body-depth))))))

(defn- process-fn-invoke-step [form bindings _trace-ctx walk-state conditional-depth]
  (when (and (seq? form) (symbol? (first form)) (seq (rest form)))
    (when-let [fn-form (get bindings (first form))]
      (when (fn-form? fn-form)
        (let [params (fn-param-syms fn-form)
              body (drop 2 fn-form)
              new-bindings (merge bindings (zipmap params (rest form)))]
          {:child {:todo body
                   :bindings new-bindings
                   :ws {:preceding nil :seen-sut? false}
                   :cd conditional-depth
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

(defn- test-line [form]
  (or (:line (meta form)) (:row (meta form))))

(defn- test-entry [form kind]
  {:form kind :test-name (second form) :body (drop 2 form) :line (test-line form)})

(defn- skip-test-form [_form pending results]
  {:pending (rest pending) :results results})

(defn- nest-test-forms [form pending results]
  {:pending (concat (rest form) (rest pending)) :results results})

(defn- collect-test-form [kind form pending results]
  {:pending (rest pending) :results (conj results (test-entry form kind))})

(def ^:private test-form-handlers
  {'deftest (partial collect-test-form :deftest)
   'it (partial collect-test-form :it)
   'describe nest-test-forms
   'context nest-test-forms})

(defn- find-tests-step [form pending results]
  (if-not (seq? form)
    (skip-test-form form pending results)
    (let [handler (get test-form-handlers (first form) skip-test-form)]
      (handler form pending results))))

(defn- find-tests [forms]
  (loop [pending (seq forms) results []]
    (if (empty? pending)
      results
      (let [{:keys [pending results]} (find-tests-step (first pending) pending results)]
        (recur pending results)))))

(defn- sut-invoke-form? [form _bindings trace-ctx]
  (trace/direct-sut-invoke-form? form trace-ctx))

(defn- form-reaches-sut? [form bindings trace-ctx]
  (or (sut-invoke-form? form bindings trace-ctx)
      (trace/reaches-sut-likely? form bindings trace-ctx)))

(defn- advance-walk-state [form bindings trace-ctx {:keys [seen-sut?] :as walk-state}]
  (cond-> walk-state
    (form-reaches-sut? form bindings trace-ctx) (assoc :seen-sut? true)
    :always (assoc :preceding form)))

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

(defn- as-conditional-assertion [result]
  (let [{:keys [verdict reason trace]} result
        underlying-reason (conditional-assertion-reason result)]
    {:verdict :conditional-assertion
     :reason underlying-reason
     :trace (assoc trace
                   :conditional? true
                   :underlying-verdict verdict
                   :underlying-reason (or reason underlying-reason))}))

(defn- finalize-assertion-result [result conditional-depth]
  (if (and result (pos? conditional-depth))
    (as-conditional-assertion result)
    result))

(defn- assertion-result
  [form bindings trace-ctx conditional-depth]
  (finalize-assertion-result
   (let [{:keys [verdict reason]} (trace/trace-form form bindings trace-ctx)]
     {:verdict verdict
      :reason reason
      :trace (trace/explain-trace form bindings trace-ctx)})
   conditional-depth))

(defn- stub-assertion-result
  [assertion-form bindings trace-ctx preceding-sut conditional-depth]
  (finalize-assertion-result
   (let [trace-target (or preceding-sut assertion-form)
         {:keys [verdict reason]} (trace/trace-form trace-target bindings trace-ctx)
         trace (assoc (trace/explain-trace trace-target bindings trace-ctx)
                        :assertion-form assertion-form
                        :preceding-sut-call preceding-sut)]
     {:verdict verdict :reason reason :trace trace})
   conditional-depth))

(defn- questionable-result [form reason conditional-depth]
  (finalize-assertion-result
   {:verdict :questionable
    :reason reason
    :trace {:assertion-form form}}
   conditional-depth))

(defn- immediate-preceding-sut? [asserted-form bindings trace-ctx preceding]
  (and preceding
       (form-reaches-sut? preceding bindings trace-ctx)
       (trace/reaches-test-module? asserted-form bindings trace-ctx)))

(defn- side-effect-evidence
  [asserted-form bindings trace-ctx {:keys [preceding seen-sut?]}]
  (when (= :introverted (:verdict (trace/trace-form asserted-form bindings trace-ctx)))
    (or (when (immediate-preceding-sut? asserted-form bindings trace-ctx preceding)
          :immediate-preceding-sut)
        (when (and seen-sut? (trace/binding-from-test-module? bindings trace-ctx))
          :test-state-binding))))

(defn- side-effect-assertion-result
  [assertion-form asserted-form bindings trace-ctx walk-state conditional-depth]
  (let [evidence (side-effect-evidence asserted-form bindings trace-ctx walk-state)
        {:keys [preceding]} walk-state
        trace-target (case evidence
                       :immediate-preceding-sut preceding
                       :test-state-binding asserted-form
                       assertion-form)]
    (finalize-assertion-result
     {:verdict :likely-extroverted
      :reason :sut-side-effect-heuristic
      :trace (cond-> (trace/explain-trace trace-target bindings trace-ctx)
               true (assoc :assertion-form assertion-form
                           :side-effect-evidence evidence)
               (= :immediate-preceding-sut evidence)
               (assoc :preceding-sut-call preceding))}
     conditional-depth)))

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

(defn- noop-step [walk-state]
  (select-keys walk-state [:preceding :seen-sut?]))

(defn- child-step [walk-state {:keys [todo bindings ws cd complete]}]
  (assoc (noop-step walk-state)
         :child {:todo todo :bindings bindings :ws ws :cd cd :complete complete}
         :results []))

(defn- result-step [walk-state results]
  (assoc (noop-step walk-state) :results results))

(defn- process-let-step [form bindings walk-state conditional-depth]
  (let [{:keys [body bindings]} (let-bindings form bindings)
        {:keys [seen-sut?]} walk-state]
    (child-step walk-state {:todo body
                            :bindings bindings
                            :ws {:preceding nil :seen-sut? seen-sut?}
                            :cd conditional-depth
                            :complete {:preceding :resume :seen-sut? :merge-or}})))

(defn- process-seq-child-step [form bindings walk-state conditional-depth complete]
  (child-step walk-state {:todo (rest form)
                          :bindings bindings
                          :ws walk-state
                          :cd conditional-depth
                          :complete complete}))

(defn- process-with-redefs-step [form bindings walk-state conditional-depth]
  (child-step walk-state {:todo (drop 2 form)
                          :bindings bindings
                          :ws walk-state
                          :cd conditional-depth
                          :complete {:preceding :child :seen-sut? :child}}))

(defn- process-fn-step [form bindings walk-state conditional-depth]
  (let [{:keys [seen-sut?]} walk-state]
    (child-step walk-state {:todo (drop 2 form)
                            :bindings bindings
                            :ws {:preceding nil :seen-sut? seen-sut?}
                            :cd conditional-depth
                            :complete {:preceding :resume :seen-sut? :merge-or}})))

(defn- process-parsed-assertion [form parsed bindings trace-ctx walk-state conditional-depth]
  (let [{:keys [preceding]} walk-state]
    (cond
      (assertions/stub-invocation? parsed)
      (result-step walk-state [(stub-assertion-result form bindings trace-ctx preceding conditional-depth)])

      (:reason parsed)
      (result-step walk-state [(questionable-result form (:reason parsed) conditional-depth)])

      (side-effect-evidence (:asserted-form parsed) bindings trace-ctx walk-state)
      (let [advanced (advance-walk-state form bindings trace-ctx walk-state)]
        (assoc (select-keys advanced [:preceding :seen-sut?])
               :results [(side-effect-assertion-result form (:asserted-form parsed) bindings
                                                        trace-ctx walk-state conditional-depth)]))

      :else
      (let [advanced (advance-walk-state form bindings trace-ctx walk-state)]
        (assoc (select-keys advanced [:preceding :seen-sut?])
               :results [(assertion-result (:asserted-form parsed) bindings trace-ctx conditional-depth)])))))

(defn- process-default-step [form bindings trace-ctx walk-state conditional-depth]
  (let [advanced (advance-walk-state form bindings trace-ctx walk-state)
        {:keys [seen-sut?]} walk-state]
    (if (form-reaches-sut? form bindings trace-ctx)
      (assoc (select-keys advanced [:preceding :seen-sut?]) :results [])
      (child-step advanced {:todo (rest form)
                            :bindings bindings
                            :ws {:preceding nil :seen-sut? seen-sut?}
                            :cd conditional-depth
                            :complete {:preceding (:preceding advanced) :seen-sut? :merge-or}}))))

(defn- process-expression-step [form bindings trace-ctx walk-state conditional-depth]
  (or (process-conditional-step form bindings trace-ctx walk-state conditional-depth)
      (process-fn-invoke-step form bindings trace-ctx walk-state conditional-depth)
      (when-let [parsed (assertions/parse-assertion form)]
        (process-parsed-assertion form parsed bindings trace-ctx walk-state conditional-depth))
      (process-default-step form bindings trace-ctx walk-state conditional-depth)))

(def ^:private seq-child-complete {:preceding :child :seen-sut? :child})

(defn- process-seq-head [form bindings walk-state conditional-depth]
  (process-seq-child-step form bindings walk-state conditional-depth seq-child-complete))

(defn- process-dotimes-head [form bindings trace-ctx walk-state conditional-depth]
  (or (process-dotimes-step form bindings trace-ctx walk-state conditional-depth)
      (assoc (noop-step walk-state) :results [])))

(def ^:private head-form-steps
  {'let process-let-step
   'loop process-let-step
   'do process-seq-head
   'try process-seq-head
   'catch process-seq-head
   'finally process-seq-head
   'with-redefs process-with-redefs-step
   'fn process-fn-step})

(defn- head-form-step [head form bindings trace-ctx walk-state conditional-depth]
  (cond
    (contains? head-form-steps head)
    ((get head-form-steps head) form bindings walk-state conditional-depth)

    (= 'doseq head)
    (result-step walk-state (process-doseq form bindings trace-ctx conditional-depth))

    (= 'dotimes head)
    (process-dotimes-head form bindings trace-ctx walk-state conditional-depth)))

(defn- process-one-form [form bindings trace-ctx walk-state conditional-depth]
  (if-not (seq? form)
    (assoc (noop-step walk-state) :results [])
    (or (head-form-step (first form) form bindings trace-ctx walk-state conditional-depth)
        (process-expression-step form bindings trace-ctx walk-state conditional-depth))))

(defn- initial-process-stack [forms bindings preceding seen-sut? conditional-depth]
  [(process-frame {:todo forms
                   :bindings bindings
                   :ws {:preceding preceding :seen-sut? seen-sut?}
                   :cd conditional-depth})])

(defn- finished-frame-result [frame results]
  (if (:resume frame)
    ::resume-parent
    {:results results
     :preceding (:preceding (:ws frame))
     :seen-sut? (:seen-sut? (:ws frame))}))

(defn- stack-after-step [stack frame step results]
  (if (:child step)
    [(push-child-frame stack (:child step)) (into results (:results step))]
    [(conj (pop stack)
           (assoc frame
                  :todo (rest (:todo frame))
                  :ws {:preceding (:preceding step)
                       :seen-sut? (:seen-sut? step)}))
     (into results (:results step))]))

(defn- process-forms-sequential
  [forms bindings trace-ctx preceding seen-sut? conditional-depth]
   (loop [stack (initial-process-stack forms bindings preceding seen-sut? conditional-depth)
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
                                        (:cd frame))
                 [stack' results'] (stack-after-step stack frame step results)]
             (recur stack' results')))))))

(defn- process-forms
  ([forms bindings trace-ctx]
   (process-forms forms bindings trace-ctx 0))
  ([forms bindings trace-ctx conditional-depth]
   (:results (process-forms-sequential forms bindings trace-ctx nil false conditional-depth))))

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
        tests (find-tests forms)
        ns-bindings (ns-fn-bindings forms)]
    (vec
     (for [{:keys [form test-name body line]} tests
           :let [results (vec (process-forms body ns-bindings trace-ctx))
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
;; {:version 1, :tested-at "2026-06-19T12:52:13.028469-05:00", :module-hash "582456547", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "-1458326996"} {:id "defn-/resolve-namespaced", :kind "defn-", :line 7, :end-line 11, :hash "1409516810"} {:id "defn-/resolve-unqualified", :kind "defn-", :line 13, :end-line 16, :hash "718817384"} {:id "defn-/resolve-ns-fn", :kind "defn-", :line 18, :end-line 23, :hash "-746974098"} {:id "defn-/destructuring-binding?", :kind "defn-", :line 25, :end-line 26, :hash "361096142"} {:id "form/5/declare", :kind "declare", :line 28, :end-line 28, :hash "-482327639"} {:id "defn-/vector-pattern-element?", :kind "defn-", :line 30, :end-line 31, :hash "-47316605"} {:id "defn-/vector-rest-index", :kind "defn-", :line 33, :end-line 34, :hash "-1649338098"} {:id "defn-/supported-vector-pattern?", :kind "defn-", :line 36, :end-line 43, :hash "-884333589"} {:id "def/map-destructure-keys", :kind "def", :line 45, :end-line 46, :hash "-599467106"} {:id "defn-/symbol-vector?", :kind "defn-", :line 48, :end-line 49, :hash "-1002205516"} {:id "defn-/standard-map-destructure-pattern?", :kind "defn-", :line 51, :end-line 56, :hash "-1501806290"} {:id "defn-/symbol-key-map-pattern?", :kind "defn-", :line 58, :end-line 59, :hash "1266033390"} {:id "defn-/supported-map-pattern?", :kind "defn-", :line 61, :end-line 63, :hash "328629814"} {:id "defn-/supported-destructure-pattern?", :kind "defn-", :line 65, :end-line 67, :hash "575065261"} {:id "defn-/unsupported-destructure-pattern?", :kind "defn-", :line 69, :end-line 71, :hash "820628602"} {:id "defn-/nth-binding", :kind "defn-", :line 73, :end-line 74, :hash "-1561782215"} {:id "defn-/drop-binding", :kind "defn-", :line 76, :end-line 77, :hash "-1024840710"} {:id "defn-/get-binding", :kind "defn-", :line 79, :end-line 80, :hash "120703912"} {:id "defn-/with-or-default", :kind "defn-", :line 82, :end-line 85, :hash "-1669426162"} {:id "form/20/declare", :kind "declare", :line 87, :end-line 87, :hash "176104591"} {:id "defn-/expand-vector-element", :kind "defn-", :line 89, :end-line 92, :hash "2078973933"} {:id "defn-/expand-vector-destructure", :kind "defn-", :line 94, :end-line 107, :hash "427439272"} {:id "defn-/map-lookup-key", :kind "defn-", :line 109, :end-line 110, :hash "223426931"} {:id "defn-/expand-symbol-key-map-destructure", :kind "defn-", :line 112, :end-line 117, :hash "1033180199"} {:id "defn-/bind-map-keys", :kind "defn-", :line 119, :end-line 124, :hash "1252663033"} {:id "defn-/bind-map-entry", :kind "defn-", :line 126, :end-line 132, :hash "55590744"} {:id "defn-/expand-map-destructure", :kind "defn-", :line 134, :end-line 140, :hash "2088334812"} {:id "defn-/expand-destructure-bindings", :kind "defn-", :line 142, :end-line 147, :hash "-866669811"} {:id "defn-/fn-form?", :kind "defn-", :line 149, :end-line 150, :hash "1534401844"} {:id "defn-/fn-param-syms", :kind "defn-", :line 152, :end-line 154, :hash "-1934413924"} {:id "defn-/doseq-coll", :kind "defn-", :line 156, :end-line 160, :hash "990849186"} {:id "form/32/declare", :kind "declare", :line 162, :end-line 162, :hash "-97545983"} {:id "defn-/bindings-for-doseq-item", :kind "defn-", :line 164, :end-line 172, :hash "54363040"} {:id "def/conditional-head-syms", :kind "def", :line 174, :end-line 176, :hash "-2029598174"} {:id "defn-/cond-branch-forms", :kind "defn-", :line 178, :end-line 185, :hash "724006868"} {:id "defn-/case-branch-forms", :kind "defn-", :line 187, :end-line 194, :hash "1041144653"} {:id "defn-/condp-branch-forms", :kind "defn-", :line 196, :end-line 197, :hash "-292336679"} {:id "defn-/if-branch-forms", :kind "defn-", :line 199, :end-line 201, :hash "-546724505"} {:id "def/conditional-branch-extractors", :kind "def", :line 203, :end-line 217, :hash "-98756298"} {:id "defn-/conditional-branch-forms", :kind "defn-", :line 219, :end-line 222, :hash "1582157075"} {:id "form/41/declare", :kind "declare", :line 224, :end-line 224, :hash "990345103"} {:id "defn-/process-frame", :kind "defn-", :line 226, :end-line 228, :hash "-1618139034"} {:id "defn-/complete-preceding", :kind "defn-", :line 230, :end-line 234, :hash "378913017"} {:id "defn-/complete-seen-sut?", :kind "defn-", :line 236, :end-line 241, :hash "1494113547"} {:id "defn-/complete-child-ws", :kind "defn-", :line 243, :end-line 245, :hash "333210935"} {:id "defn-/push-child-frame", :kind "defn-", :line 247, :end-line 250, :hash "687311338"} {:id "defn-/resume-parent-frame", :kind "defn-", :line 252, :end-line 253, :hash "873158305"} {:id "defn-/process-conditional-step", :kind "defn-", :line 255, :end-line 264, :hash "-372439797"} {:id "defn-/process-dotimes-step", :kind "defn-", :line 266, :end-line 278, :hash "1438604733"} {:id "defn-/process-doseq", :kind "defn-", :line 280, :end-line 295, :hash "-2077598064"} {:id "defn-/process-fn-invoke-step", :kind "defn-", :line 297, :end-line 311, :hash "1847453905"} {:id "defn-/defn-arity-fn-literal", :kind "defn-", :line 313, :end-line 314, :hash "7584441"} {:id "defn-/defn-docstring-fn-literal", :kind "defn-", :line 316, :end-line 317, :hash "-396941995"} {:id "defn-/defn-docstring?", :kind "defn-", :line 319, :end-line 320, :hash "1220406710"} {:id "defn-/defn->fn-literal", :kind "defn-", :line 322, :end-line 328, :hash "-397160554"} {:id "defn-/ns-fn-bindings", :kind "defn-", :line 330, :end-line 335, :hash "656755951"} {:id "defn-/test-line", :kind "defn-", :line 337, :end-line 338, :hash "-1424048643"} {:id "defn-/test-entry", :kind "defn-", :line 340, :end-line 341, :hash "-1272399894"} {:id "defn-/skip-test-form", :kind "defn-", :line 343, :end-line 344, :hash "-242367102"} {:id "defn-/nest-test-forms", :kind "defn-", :line 346, :end-line 347, :hash "-1321991206"} {:id "defn-/collect-test-form", :kind "defn-", :line 349, :end-line 350, :hash "1521910597"} {:id "def/test-form-handlers", :kind "def", :line 352, :end-line 356, :hash "-1760196749"} {:id "defn-/find-tests-step", :kind "defn-", :line 358, :end-line 362, :hash "1864902714"} {:id "defn-/find-tests", :kind "defn-", :line 364, :end-line 369, :hash "722202088"} {:id "defn-/sut-invoke-form?", :kind "defn-", :line 371, :end-line 372, :hash "966618037"} {:id "defn-/form-reaches-sut?", :kind "defn-", :line 374, :end-line 376, :hash "30025062"} {:id "defn-/advance-walk-state", :kind "defn-", :line 378, :end-line 381, :hash "569498286"} {:id "def/underlying-conditional-reasons", :kind "def", :line 383, :end-line 385, :hash "1473226787"} {:id "defn-/introverted-conditional-reason", :kind "defn-", :line 387, :end-line 388, :hash "2062598749"} {:id "defn-/conditional-assertion-reason", :kind "defn-", :line 390, :end-line 394, :hash "1948739796"} {:id "defn-/as-conditional-assertion", :kind "defn-", :line 396, :end-line 404, :hash "1468502162"} {:id "defn-/finalize-assertion-result", :kind "defn-", :line 406, :end-line 409, :hash "-1616844118"} {:id "defn-/assertion-result", :kind "defn-", :line 411, :end-line 418, :hash "1357394087"} {:id "defn-/stub-assertion-result", :kind "defn-", :line 420, :end-line 429, :hash "702825486"} {:id "defn-/questionable-result", :kind "defn-", :line 431, :end-line 436, :hash "266969877"} {:id "defn-/immediate-preceding-sut?", :kind "defn-", :line 438, :end-line 441, :hash "535255210"} {:id "defn-/side-effect-evidence", :kind "defn-", :line 443, :end-line 449, :hash "-1255916380"} {:id "defn-/side-effect-assertion-result", :kind "defn-", :line 451, :end-line 467, :hash "-1239846332"} {:id "defn-/build-finding-trace", :kind "defn-", :line 469, :end-line 474, :hash "377525482"} {:id "defn-/let-binding-pairs", :kind "defn-", :line 476, :end-line 482, :hash "-1105506548"} {:id "defn-/assoc-let-pair", :kind "defn-", :line 484, :end-line 488, :hash "699566064"} {:id "defn-/let-bindings", :kind "defn-", :line 490, :end-line 495, :hash "-161395392"} {:id "defn-/noop-step", :kind "defn-", :line 497, :end-line 498, :hash "-914493725"} {:id "defn-/child-step", :kind "defn-", :line 500, :end-line 503, :hash "230335217"} {:id "defn-/result-step", :kind "defn-", :line 505, :end-line 506, :hash "1574422238"} {:id "defn-/process-let-step", :kind "defn-", :line 508, :end-line 515, :hash "-1620493156"} {:id "defn-/process-seq-child-step", :kind "defn-", :line 517, :end-line 522, :hash "1850627445"} {:id "defn-/process-with-redefs-step", :kind "defn-", :line 524, :end-line 529, :hash "351004739"} {:id "defn-/process-fn-step", :kind "defn-", :line 531, :end-line 537, :hash "821783368"} {:id "defn-/process-parsed-assertion", :kind "defn-", :line 539, :end-line 557, :hash "683838359"} {:id "defn-/process-default-step", :kind "defn-", :line 559, :end-line 568, :hash "719800108"} {:id "defn-/process-expression-step", :kind "defn-", :line 570, :end-line 575, :hash "123233305"} {:id "def/seq-child-complete", :kind "def", :line 577, :end-line 577, :hash "-1727835598"} {:id "defn-/process-seq-head", :kind "defn-", :line 579, :end-line 580, :hash "-1615335996"} {:id "defn-/process-dotimes-head", :kind "defn-", :line 582, :end-line 584, :hash "-2036225986"} {:id "def/head-form-steps", :kind "def", :line 586, :end-line 594, :hash "302488026"} {:id "defn-/head-form-step", :kind "defn-", :line 596, :end-line 605, :hash "-593201190"} {:id "defn-/process-one-form", :kind "defn-", :line 607, :end-line 611, :hash "-2079314240"} {:id "defn-/initial-process-stack", :kind "defn-", :line 613, :end-line 617, :hash "-1531595454"} {:id "defn-/finished-frame-result", :kind "defn-", :line 619, :end-line 624, :hash "1782626509"} {:id "defn-/stack-after-step", :kind "defn-", :line 626, :end-line 634, :hash "-308345359"} {:id "defn-/process-forms-sequential", :kind "defn-", :line 636, :end-line 654, :hash "-1610871595"} {:id "defn-/process-forms", :kind "defn-", :line 656, :end-line 660, :hash "-420498085"} {:id "defn-/body-form", :kind "defn-", :line 662, :end-line 665, :hash "576180413"} {:id "defn-/promote-cloistered", :kind "defn-", :line 667, :end-line 671, :hash "1925045456"} {:id "defn-/unconditional-assertion-results", :kind "defn-", :line 673, :end-line 674, :hash "1005685673"} {:id "defn-/strongest-conditional-reason", :kind "defn-", :line 676, :end-line 682, :hash "-110716430"} {:id "defn-/first-unconditional-verdict", :kind "defn-", :line 684, :end-line 685, :hash "1690687522"} {:id "defn-/extroverted-verdict", :kind "defn-", :line 687, :end-line 689, :hash "976179155"} {:id "defn-/likely-extroverted-verdict", :kind "defn-", :line 691, :end-line 694, :hash "-501801606"} {:id "defn-/questionable-verdict", :kind "defn-", :line 696, :end-line 698, :hash "566982830"} {:id "defn-/introverted-verdict", :kind "defn-", :line 700, :end-line 702, :hash "1774438313"} {:id "defn-/unconditional-verdict", :kind "defn-", :line 704, :end-line 708, :hash "1145335451"} {:id "defn-/test-verdict", :kind "defn-", :line 710, :end-line 715, :hash "212935738"} {:id "defn-/findings-for-forms", :kind "defn-", :line 717, :end-line 745, :hash "391708565"} {:id "defn/analyze-forms", :kind "defn", :line 747, :end-line 750, :hash "-169972165"} {:id "defn/analyze-file", :kind "defn", :line 752, :end-line 756, :hash "1315649154"}]}
;; clj-mutate-manifest-end
