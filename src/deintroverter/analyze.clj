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

(defn- expand-map-destructure [pattern rhs bindings]
  (if (symbol-key-map-pattern? pattern)
    (expand-symbol-key-map-destructure pattern rhs bindings)
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

(defn- conditional-branch-forms [form]
  (when (and (seq? form) (contains? conditional-head-syms (first form)))
    (case (first form)
      when (drop 1 form)
      when-not (drop 1 form)
      if (let [parts (rest form)]
           (remove nil? [(nth parts 1 nil) (nth parts 2 nil)]))
      cond (cond-branch-forms (rest form))
      case (case-branch-forms form)
      case+ (case-branch-forms form)
      condp (condp-branch-forms form)
      and (rest form)
      or (rest form)
      if-let (drop 2 form)
      when-let (drop 2 form)
      when-some (drop 2 form)
      if-some (drop 2 form)
      while (list (nth form 2)))))

(declare process-forms-sequential)

(defn- process-frame
  [{:keys [todo bindings ws cd resume complete]}]
  {:todo (seq todo) :bindings bindings :ws ws :cd cd :resume resume :complete complete})

(defn- complete-child-ws [child-frame]
  (let [{:keys [ws resume complete]} child-frame
        resume-ws (:ws resume)]
    {:preceding (case (:preceding complete)
                  :child (:preceding ws)
                  :resume (:preceding resume-ws)
                  (:preceding complete))
     :seen-sut? (case (:seen-sut? complete)
                  :child (:seen-sut? ws)
                  :resume (:seen-sut? resume-ws)
                  :merge-or (or (:seen-sut? resume-ws) (:seen-sut? ws)))}))

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

(defn- defn->fn-literal [form]
  (when (and (seq? form) (#{'defn 'defn-} (first form)) (>= (count form) 4))
    (let [third (nth form 2)]
      (cond
        (vector? third)
        (list 'fn third (nth form 3))

        (and (string? third) (>= (count form) 5) (vector? (nth form 3)))
        (list 'fn (nth form 3) (nth form 4))

        :else nil))))

(defn- ns-fn-bindings [forms]
  (into {}
        (keep (fn [form]
                (when-let [fn-lit (defn->fn-literal form)]
                  [(second form) fn-lit]))
              forms)))

(defn- find-tests [forms]
  (loop [pending (seq forms) results []]
    (if (empty? pending)
      results
      (let [form (first pending)]
        (if-not (seq? form)
          (recur (rest pending) results)
          (case (first form)
            deftest (recur (rest pending)
                           (conj results
                                 {:form :deftest :test-name (second form) :body (drop 2 form)
                                  :line (or (:line (meta form)) (:row (meta form)))}))
            it (recur (rest pending)
                      (conj results
                            {:form :it :test-name (second form) :body (drop 2 form)
                             :line (or (:line (meta form)) (:row (meta form)))}))
            describe (recur (concat (rest form) (rest pending)) results)
            context (recur (concat (rest form) (rest pending)) results)
            (recur (rest pending) results)))))))

(defn- sut-invoke-form? [form _bindings trace-ctx]
  (trace/direct-sut-invoke-form? form trace-ctx))

(defn- form-reaches-sut? [form bindings trace-ctx]
  (or (sut-invoke-form? form bindings trace-ctx)
      (trace/reaches-sut-likely? form bindings trace-ctx)))

(defn- advance-walk-state [form bindings trace-ctx {:keys [seen-sut?] :as walk-state}]
  (cond-> walk-state
    (form-reaches-sut? form bindings trace-ctx) (assoc :seen-sut? true)
    :always (assoc :preceding form)))

(defn- conditional-assertion-reason [{:keys [verdict reason]}]
  (cond
    (= verdict :extroverted) :would-be-extroverted
    (= verdict :likely-extroverted) :would-be-likely-extroverted
    (= verdict :introverted) (or reason :no-sut-assertion)
    (= verdict :questionable) reason
    :else :conditional-assertion))

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

(defn- side-effect-evidence
  [asserted-form bindings trace-ctx {:keys [preceding seen-sut?]}]
  (let [{:keys [verdict]} (trace/trace-form asserted-form bindings trace-ctx)]
    (when (= :introverted verdict)
      (cond
        (and preceding (form-reaches-sut? preceding bindings trace-ctx)
             (trace/reaches-test-module? asserted-form bindings trace-ctx))
        :immediate-preceding-sut

        (and seen-sut? (trace/binding-from-test-module? bindings trace-ctx))
        :test-state-binding

        :else nil))))

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

(defn- process-one-form [form bindings trace-ctx walk-state conditional-depth]
  (let [{:keys [preceding seen-sut?]} walk-state
        noop-step {:results [] :preceding preceding :seen-sut? seen-sut?}]
    (cond
      (not (seq? form))
      noop-step

      (#{'let 'loop} (first form))
      (let [{:keys [body bindings]} (let-bindings form bindings)]
        {:child {:todo body
                 :bindings bindings
                 :ws {:preceding nil :seen-sut? seen-sut?}
                 :cd conditional-depth
                 :complete {:preceding :resume :seen-sut? :merge-or}}
         :results []
         :preceding preceding
         :seen-sut? seen-sut?})

      (#{'do 'try 'catch 'finally} (first form))
      {:child {:todo (rest form)
               :bindings bindings
               :ws walk-state
               :cd conditional-depth
               :complete {:preceding :child :seen-sut? :child}}
       :results []
       :preceding preceding
       :seen-sut? seen-sut?}

      (= 'with-redefs (first form))
      {:child {:todo (drop 2 form)
               :bindings bindings
               :ws walk-state
               :cd conditional-depth
               :complete {:preceding :child :seen-sut? :child}}
       :results []
       :preceding preceding
       :seen-sut? seen-sut?}

      (= 'doseq (first form))
      {:results (process-doseq form bindings trace-ctx conditional-depth)
       :preceding preceding
       :seen-sut? seen-sut?}

      (= 'dotimes (first form))
      (or (process-dotimes-step form bindings trace-ctx walk-state conditional-depth)
          noop-step)

      (= 'fn (first form))
      {:child {:todo (drop 2 form)
               :bindings bindings
               :ws {:preceding nil :seen-sut? seen-sut?}
               :cd conditional-depth
               :complete {:preceding :resume :seen-sut? :merge-or}}
       :results []
       :preceding preceding
       :seen-sut? seen-sut?}

      :else
      (or (process-conditional-step form bindings trace-ctx walk-state conditional-depth)
          (process-fn-invoke-step form bindings trace-ctx walk-state conditional-depth)
          (let [parsed (assertions/parse-assertion form)]
            (cond
              (and parsed (assertions/stub-invocation? parsed))
              {:results [(stub-assertion-result form bindings trace-ctx preceding conditional-depth)]
               :preceding preceding
               :seen-sut? seen-sut?}

              (and parsed (:reason parsed))
              {:results [(questionable-result form (:reason parsed) conditional-depth)]
               :preceding preceding
               :seen-sut? seen-sut?}

              (and parsed (side-effect-evidence (:asserted-form parsed) bindings trace-ctx walk-state))
              (let [advanced (advance-walk-state form bindings trace-ctx walk-state)]
                {:results [(side-effect-assertion-result form (:asserted-form parsed) bindings
                                                          trace-ctx walk-state conditional-depth)]
                 :preceding (:preceding advanced)
                 :seen-sut? (:seen-sut? advanced)})

              parsed
              (let [advanced (advance-walk-state form bindings trace-ctx walk-state)]
                {:results [(assertion-result (:asserted-form parsed) bindings trace-ctx conditional-depth)]
                 :preceding (:preceding advanced)
                 :seen-sut? (:seen-sut? advanced)})

              (form-reaches-sut? form bindings trace-ctx)
              (let [advanced (advance-walk-state form bindings trace-ctx walk-state)]
                {:results []
                 :preceding (:preceding advanced)
                 :seen-sut? (:seen-sut? advanced)})

              :else
              (let [advanced (advance-walk-state form bindings trace-ctx walk-state)]
                {:child {:todo (rest form)
                         :bindings bindings
                         :ws {:preceding nil :seen-sut? seen-sut?}
                         :cd conditional-depth
                         :complete {:preceding (:preceding advanced) :seen-sut? :merge-or}}
                 :results []
                 :preceding (:preceding advanced)
                 :seen-sut? (:seen-sut? advanced)})))))))

(defn- process-forms-sequential
  ([forms bindings trace-ctx]
   (process-forms-sequential forms bindings trace-ctx nil false 0))
  ([forms bindings trace-ctx preceding seen-sut?]
   (process-forms-sequential forms bindings trace-ctx preceding seen-sut? 0))
  ([forms bindings trace-ctx preceding seen-sut? conditional-depth]
   (loop [stack [(process-frame {:todo forms
                                  :bindings bindings
                                  :ws {:preceding preceding :seen-sut? seen-sut?}
                                  :cd conditional-depth})]
          results []]
     (if (empty? stack)
       {:results results :preceding preceding :seen-sut? seen-sut?}
       (let [frame (peek stack)]
         (if (empty? (:todo frame))
           (if (:resume frame)
             (recur (resume-parent-frame stack frame) results)
             {:results results
              :preceding (:preceding (:ws frame))
              :seen-sut? (:seen-sut? (:ws frame))})
           (let [step (process-one-form (first (:todo frame))
                                        (:bindings frame)
                                        trace-ctx
                                        (:ws frame)
                                        (:cd frame))]
             (if (:child step)
               (recur (push-child-frame stack (:child step))
                      (into results (:results step)))
               (recur (conj (pop stack)
                            (assoc frame
                                   :todo (rest (:todo frame))
                                   :ws {:preceding (:preceding step)
                                        :seen-sut? (:seen-sut? step)}))
                      (into results (:results step)))))))))))

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

(defn- test-verdict [results]
  (let [unconditional (unconditional-assertion-results results)]
    (cond
      (some #(= :extroverted (:verdict %)) unconditional)
      {:verdict :extroverted :reason nil}

      (some #(= :likely-extroverted (:verdict %)) unconditional)
      (let [match (first (filter #(= :likely-extroverted (:verdict %)) unconditional))]
        {:verdict :likely-extroverted
         :reason (or (:reason match) :refer-all-heuristic)})

      (some #(= :questionable (:verdict %)) unconditional)
      {:verdict :questionable
       :reason (or (:reason (first (filter #(= :questionable (:verdict %)) unconditional)))
                   :unknown)}

      (seq unconditional)
      {:verdict :introverted :reason :no-sut-assertion}

      (some #(= :conditional-assertion (:verdict %)) results)
      {:verdict :conditional-assertion
       :reason (strongest-conditional-reason results)}

      :else
      {:verdict :introverted :reason :no-assertions})))

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
        tests   (find-tests forms)
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