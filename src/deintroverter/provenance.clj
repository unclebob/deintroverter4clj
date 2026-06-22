(ns deintroverter.provenance
  (:require [deintroverter.forms :as forms]
            [deintroverter.trace :as trace]))

(def ^:private max-derivation-depth 3)

(def catch-exception-marker '__catch-exception__)

(def ^:private linkable-kinds
  #{:sut-invoke :sut-derived :catch-derived})

(def ^:private kind-priority
  {:sut-invoke 0
   :catch-derived 1
   :sut-derived 2
   :fixture 3
   :stub-capture 4
   :world-mutation 5
   :test-module 6
   :literal 7
   :unknown 8})

(defn literal-provenance
  [form]
  {:kind :literal :source form :confidence :proven :via []})

(defn sut-invoke-provenance
  [form confidence]
  {:kind :sut-invoke :source form :confidence confidence :via []})

(defn catch-derived-provenance
  [form]
  {:kind :catch-derived :source form :confidence :proven :via []})

(defn fixture-provenance
  [form]
  {:kind :fixture :source form :confidence :proven :via []})

(defn- unknown-provenance
  [form]
  {:kind :unknown :source form :confidence :proven :via []})

(defn provenance-kind
  [prov]
  (:kind prov))

(defn linkable-kind?
  [kind]
  (contains? linkable-kinds kind))

(defn- confidence-rank
  [confidence]
  (if (= confidence :proven) 0 1))

(defn- stronger-provenance
  [a b]
  (if (<= (get kind-priority (:kind a) 8)
          (get kind-priority (:kind b) 8))
    [a b]
    [b a]))

(defn- merged-confidence
  [primary secondary]
  (if (<= (confidence-rank (:confidence primary))
          (confidence-rank (:confidence secondary)))
    (:confidence primary)
    (:confidence secondary)))

(defn merge-provenance
  "Prefer the stronger kind and confidence; concatenate :via chains."
  [a b]
  (cond
    (nil? a) b
    (nil? b) a
    :else
    (let [[primary secondary] (stronger-provenance a b)]
      {:kind (:kind primary)
       :source (:source primary)
       :confidence (merged-confidence primary secondary)
       :via (vec (concat (:via primary []) (:via secondary [])))})))

(defn legacy-evidence-tag
  "Map a provenance kind to a legacy analyze evidence keyword."
  [prov & [{:keys [nested?]}]]
  (case (:kind prov)
    :sut-derived :sut-result-read
    :catch-derived :exception-catch-assertion
    :sut-invoke (if nested? :nested-sut-invoke :sut-invoke)
    nil))

(defn- literal-form?
  [form]
  (or (string? form) (number? form) (keyword? form) (boolean? form)
      (nil? form) (char? form)
      (and (coll? form) (not (seq? form))
           (every? literal-form? form))))

(defn- fn-form?
  [form]
  (and (seq? form) (#{'fn 'fn*} (first form))))

(defn- fn-param-syms
  [fn-form]
  (let [params (second fn-form)]
    (if (vector? params) (vec params) (vector (first params)))))

(defn- helper-fn-body
  [fn-form]
  (let [body (drop 2 fn-form)]
    (if (= 1 (count body))
      (first body)
      (cons 'do body))))

(defn- invoke-arg-value
  [arg bindings]
  (if (and (symbol? arg) (contains? bindings arg))
    (get bindings arg)
    arg))

(defn- deref-form?
  [form]
  (and (seq? form) (= 2 (count form))
       (#{'deref 'clojure.core/deref} (first form))))

(defn- get-form?
  [form]
  (and (seq? form)
       (#{'get 'clojure.core/get} (first form))
       (>= (count form) 2)))

(defn- ex-data-form?
  [form]
  (and (seq? form) (= 'ex-data (first form)) (= 2 (count form))))

(defn- var-deref-form?
  [form]
  (and (deref-form? form)
       (let [arg (second form)]
         (and (seq? arg) (= 'var (first arg)) (symbol? (second arg))))))

(defn- sut-confidence
  [form bindings trace-ctx]
  (cond
    (trace/reaches-sut? form bindings trace-ctx) :proven
    (trace/reaches-sut-likely? form bindings trace-ctx) :likely
    :else nil))

(defn- var-deref-sut-invoke?
  [form trace-ctx]
  (when (var-deref-form? form)
    (let [var-sym (second (second form))]
      (when (trace/reaches-sut? var-sym {} trace-ctx)
        :proven))))

(defn- helper-invoke-sut-confidence
  [form bindings trace-ctx]
  (when (and (seq? form) (symbol? (first form)))
    (when-let [fn-form (get bindings (first form))]
      (when (fn-form? fn-form)
        (let [params (fn-param-syms fn-form)
              arg-bindings (merge bindings
                                  (zipmap params
                                          (map #(invoke-arg-value % bindings)
                                               (rest form))))]
          (sut-confidence (helper-fn-body fn-form) arg-bindings trace-ctx))))))

(declare derive-provenance)

(defn- binding-prov
  [sym opts]
  (get (:binding-provs opts) sym))

(defn- prov-from-symbol
  [sym bindings trace-ctx opts]
  (or (binding-prov sym opts)
      (when-let [value (trace/resolve-bound-form sym bindings)]
        (when (not= value sym)
          (derive-provenance value bindings trace-ctx
                             (update opts :depth (fnil inc 0)))))))

(defn- propagate-provenance
  [expr base-prov via-step]
  (let [via (conj (:via base-prov) via-step)]
    (if (> (count via) max-derivation-depth)
      (unknown-provenance expr)
      (-> base-prov
          (assoc :kind :sut-derived
                 :source expr
                 :via via)))))

(defn- derive-from-get
  [expr bindings trace-ctx opts]
  (when (get-form? expr)
    (let [coll-prov (derive-provenance (second expr) bindings trace-ctx opts)]
      (when (#{:sut-invoke :sut-derived :catch-derived} (:kind coll-prov))
        (propagate-provenance expr coll-prov :get)))))

(defn- catch-exception-sym?
  [sym bindings]
  (= catch-exception-marker (get bindings sym)))

(defn- sut-result-rhs-form [v]
  (if (and (seq? v) (= 'get (first v)) (>= (count v) 2))
    (nth v 1)
    v))

(defn- binding-value-reaches-sut? [v bindings trace-ctx]
  (or (some? (sut-confidence v bindings trace-ctx))
      (some? (helper-invoke-sut-confidence v bindings trace-ctx))
      (trace/namespaced-production-invoke? v bindings trace-ctx)))

(defn- let-bound-sut-result-sym?
  [sym bindings trace-ctx]
  (when (and (symbol? sym) (contains? bindings sym))
    (binding-value-reaches-sut? (sut-result-rhs-form (get bindings sym))
                                bindings trace-ctx)))

(defn- catch-derived-arg?
  [arg bindings opts]
  (or (and (symbol? arg)
           (= :catch-derived (:kind (binding-prov arg opts))))
      (catch-exception-sym? arg bindings)))

(defn- derive-from-ex-data
  [expr bindings opts]
  (when (ex-data-form? expr)
    (when (catch-derived-arg? (second expr) bindings opts)
      (catch-derived-provenance expr))))

(defn- deref-symbol-provenance
  [expr target bindings trace-ctx opts]
  (when-let [target-prov (prov-from-symbol target bindings trace-ctx opts)]
    (when (not= :unknown (:kind target-prov))
      (assoc target-prov
             :source expr
             :via (conj (:via target-prov) :deref)))))

(defn- derive-from-deref
  [expr bindings trace-ctx opts]
  (when (deref-form? expr)
    (or (when (var-deref-sut-invoke? expr trace-ctx)
          (sut-invoke-provenance expr :proven))
        (deref-symbol-provenance expr (second expr) bindings trace-ctx opts))))

(defn- derive-from-invoke
  [expr bindings trace-ctx opts]
  (or (when-let [confidence (sut-confidence expr bindings trace-ctx)]
        (sut-invoke-provenance expr confidence))
      (when-let [confidence (helper-invoke-sut-confidence expr bindings trace-ctx)]
        (sut-invoke-provenance expr confidence))
      (when (trace/reaches-test-module? expr bindings trace-ctx)
        {:kind :test-module :source expr :confidence :proven :via []})))

(defn- derive-composite-provenance
  [expr bindings trace-ctx opts]
  (or (derive-from-get expr bindings trace-ctx opts)
      (derive-from-ex-data expr bindings opts)
      (derive-from-deref expr bindings trace-ctx opts)
      (derive-from-invoke expr bindings trace-ctx opts)
      (unknown-provenance expr)))

(defn- derive-by-shape
  [expr bindings trace-ctx opts]
  (cond
    (literal-form? expr) (literal-provenance expr)
    (symbol? expr) (or (prov-from-symbol expr bindings trace-ctx opts)
                       (unknown-provenance expr))
    :else (derive-composite-provenance expr bindings trace-ctx opts)))

(defn derive-provenance
  "Derive provenance for an expression given flat bindings and trace context.
  opts may include :binding-provs, :depth."
  [expr bindings trace-ctx & [opts]]
  (let [opts (or opts {})]
    (if (> (:depth opts 0) max-derivation-depth)
      (unknown-provenance expr)
      (derive-by-shape expr bindings trace-ctx opts))))

(defn- resolve-asserted-subject
  [form bindings]
  (trace/resolve-bound-form form bindings))

(defn- catch-linkable?
  [prov opts walk-state]
  (if (and (= :catch-derived (:kind prov))
           (:require-seen-sut? opts))
    (:seen-sut? walk-state)
    true))

(defn- linkable-provenance?
  [prov opts walk-state]
  (and (linkable-kind? (:kind prov))
       (catch-linkable? prov opts walk-state)))

(defn- catch-sym-link-candidate [sym bindings opts walk-state]
  (when (catch-exception-sym? sym bindings)
    (let [prov (catch-derived-provenance sym)]
      (when (catch-linkable? prov opts walk-state)
        prov))))

(defn- binding-sym-link-candidate [sym bindings trace-ctx opts walk-state]
  (when-let [prov (or (binding-prov sym opts)
                      (prov-from-symbol sym bindings trace-ctx opts))]
    (when (and (linkable-provenance? prov opts walk-state)
               (or (not= :sut-derived (:kind prov))
                   (let-bound-sut-result-sym? sym bindings trace-ctx)))
      prov)))

(defn- sym-link-candidate [sym bindings trace-ctx opts walk-state]
  (or (catch-sym-link-candidate sym bindings opts walk-state)
      (binding-sym-link-candidate sym bindings trace-ctx opts walk-state)))

(defn- sym-link-candidates
  [asserted-form bindings trace-ctx opts walk-state]
  (keep #(sym-link-candidate % bindings trace-ctx opts walk-state)
        (forms/symbols-in-form asserted-form)))

(defn- legacy-sut-result-read-link
  [asserted-form bindings trace-ctx]
  (when-let [sym (first (filter #(let-bound-sut-result-sym? % bindings trace-ctx)
                                 (forms/symbols-in-form asserted-form)))]
    {:linked? true
     :kind :sut-derived
     :source (trace/resolve-bound-form sym bindings)
     :legacy-evidence :sut-result-read}))

(defn- form-link-candidates
  [asserted-form bindings trace-ctx opts walk-state]
  (keep (fn [subform]
          (let [prov (derive-provenance subform bindings trace-ctx opts)]
            (when (linkable-provenance? prov opts walk-state)
              prov)))
        (forms/collect-seq-forms asserted-form)))

(defn- nested-sut-invoke?
  [asserted-form bindings trace-ctx opts]
  (let [subject (resolve-asserted-subject asserted-form bindings)]
    (boolean
     (some (fn [subform]
             (and (not= subform subject)
                  (= :sut-invoke
                     (:kind (derive-provenance subform bindings trace-ctx opts)))))
           (forms/collect-seq-forms subject)))))

(defn- link-result
  [prov asserted-form bindings trace-ctx opts]
  {:linked? true
   :kind (:kind prov)
   :source (:source prov)
   :legacy-evidence (legacy-evidence-tag
                     prov
                     {:nested? (and (= :sut-invoke (:kind prov))
                                    (nested-sut-invoke? asserted-form
                                                        bindings
                                                        trace-ctx
                                                        opts))})})

(defn provenance-link?
  "Return link metadata when asserted-form resolves to SUT-linked provenance.
  opts may include :binding-provs, :walk-state, :require-seen-sut?."
  [asserted-form bindings trace-ctx & [opts]]
  (let [opts (or opts {})
        walk-state (:walk-state opts {})]
    (or (legacy-sut-result-read-link asserted-form bindings trace-ctx)
        (when-let [prov (first (sort-by #(get kind-priority (:kind %) 8)
                                        (concat (sym-link-candidates asserted-form
                                                                     bindings
                                                                     trace-ctx
                                                                     opts
                                                                     walk-state)
                                                (form-link-candidates asserted-form
                                                                    bindings
                                                                    trace-ctx
                                                                    opts
                                                                    walk-state))))]
          (link-result prov asserted-form bindings trace-ctx opts)))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-22T09:10:43.977641-05:00", :module-hash "834130155", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "-1133993360"} {:id "def/max-derivation-depth", :kind "def", :line 5, :end-line 5, :hash "-1734920150"} {:id "def/catch-exception-marker", :kind "def", :line 7, :end-line 7, :hash "-1506975183"} {:id "def/linkable-kinds", :kind "def", :line 9, :end-line 10, :hash "973066871"} {:id "def/kind-priority", :kind "def", :line 12, :end-line 21, :hash "-1520667133"} {:id "defn/literal-provenance", :kind "defn", :line 23, :end-line 25, :hash "-77980544"} {:id "defn/sut-invoke-provenance", :kind "defn", :line 27, :end-line 29, :hash "1044422148"} {:id "defn/catch-derived-provenance", :kind "defn", :line 31, :end-line 33, :hash "-672991908"} {:id "defn/fixture-provenance", :kind "defn", :line 35, :end-line 37, :hash "-630199751"} {:id "defn-/unknown-provenance", :kind "defn-", :line 39, :end-line 41, :hash "-1494765251"} {:id "defn/provenance-kind", :kind "defn", :line 43, :end-line 45, :hash "-529827547"} {:id "defn/linkable-kind?", :kind "defn", :line 47, :end-line 49, :hash "1915260699"} {:id "defn-/confidence-rank", :kind "defn-", :line 51, :end-line 53, :hash "-1054878421"} {:id "defn-/stronger-provenance", :kind "defn-", :line 55, :end-line 60, :hash "-1733265161"} {:id "defn-/merged-confidence", :kind "defn-", :line 62, :end-line 67, :hash "-165876956"} {:id "defn/merge-provenance", :kind "defn", :line 69, :end-line 80, :hash "-1144992085"} {:id "defn/legacy-evidence-tag", :kind "defn", :line 82, :end-line 89, :hash "-1196022498"} {:id "defn-/literal-form?", :kind "defn-", :line 91, :end-line 96, :hash "-306474601"} {:id "defn-/fn-form?", :kind "defn-", :line 98, :end-line 100, :hash "1534401844"} {:id "defn-/fn-param-syms", :kind "defn-", :line 102, :end-line 105, :hash "-1934413924"} {:id "defn-/helper-fn-body", :kind "defn-", :line 107, :end-line 112, :hash "2097135909"} {:id "defn-/invoke-arg-value", :kind "defn-", :line 114, :end-line 118, :hash "1400582230"} {:id "defn-/deref-form?", :kind "defn-", :line 120, :end-line 123, :hash "-1247135817"} {:id "defn-/get-form?", :kind "defn-", :line 125, :end-line 129, :hash "1140265387"} {:id "defn-/ex-data-form?", :kind "defn-", :line 131, :end-line 133, :hash "173114652"} {:id "defn-/var-deref-form?", :kind "defn-", :line 135, :end-line 139, :hash "2096069336"} {:id "defn-/sut-confidence", :kind "defn-", :line 141, :end-line 146, :hash "-2000657207"} {:id "defn-/var-deref-sut-invoke?", :kind "defn-", :line 148, :end-line 153, :hash "-1858389628"} {:id "defn-/helper-invoke-sut-confidence", :kind "defn-", :line 155, :end-line 165, :hash "2058149619"} {:id "form/29/declare", :kind "declare", :line 167, :end-line 167, :hash "1151156287"} {:id "defn-/binding-prov", :kind "defn-", :line 169, :end-line 171, :hash "-1006990279"} {:id "defn-/prov-from-symbol", :kind "defn-", :line 173, :end-line 179, :hash "-103587318"} {:id "defn-/propagate-provenance", :kind "defn-", :line 181, :end-line 189, :hash "-1412729748"} {:id "defn-/derive-from-get", :kind "defn-", :line 191, :end-line 196, :hash "-429614726"} {:id "defn-/catch-exception-sym?", :kind "defn-", :line 198, :end-line 200, :hash "1344367517"} {:id "defn-/sut-result-rhs-form", :kind "defn-", :line 202, :end-line 205, :hash "-855561097"} {:id "defn-/binding-value-reaches-sut?", :kind "defn-", :line 207, :end-line 210, :hash "-421558763"} {:id "defn-/let-bound-sut-result-sym?", :kind "defn-", :line 212, :end-line 216, :hash "240367100"} {:id "defn-/catch-derived-arg?", :kind "defn-", :line 218, :end-line 222, :hash "402932462"} {:id "defn-/derive-from-ex-data", :kind "defn-", :line 224, :end-line 228, :hash "-397455541"} {:id "defn-/deref-symbol-provenance", :kind "defn-", :line 230, :end-line 236, :hash "2059451465"} {:id "defn-/derive-from-deref", :kind "defn-", :line 238, :end-line 243, :hash "-760955689"} {:id "defn-/derive-from-invoke", :kind "defn-", :line 245, :end-line 252, :hash "-1788600061"} {:id "defn-/derive-composite-provenance", :kind "defn-", :line 254, :end-line 260, :hash "-1148541332"} {:id "defn-/derive-by-shape", :kind "defn-", :line 262, :end-line 268, :hash "1686941186"} {:id "defn/derive-provenance", :kind "defn", :line 270, :end-line 277, :hash "-1058278112"} {:id "defn-/resolve-asserted-subject", :kind "defn-", :line 279, :end-line 281, :hash "844655974"} {:id "defn-/catch-linkable?", :kind "defn-", :line 283, :end-line 288, :hash "-637284386"} {:id "defn-/linkable-provenance?", :kind "defn-", :line 290, :end-line 293, :hash "849389308"} {:id "defn-/catch-sym-link-candidate", :kind "defn-", :line 295, :end-line 299, :hash "1038921862"} {:id "defn-/binding-sym-link-candidate", :kind "defn-", :line 301, :end-line 307, :hash "1468982145"} {:id "defn-/sym-link-candidate", :kind "defn-", :line 309, :end-line 311, :hash "-11278312"} {:id "defn-/sym-link-candidates", :kind "defn-", :line 313, :end-line 316, :hash "-982603022"} {:id "defn-/legacy-sut-result-read-link", :kind "defn-", :line 318, :end-line 325, :hash "1076501695"} {:id "defn-/form-link-candidates", :kind "defn-", :line 327, :end-line 333, :hash "-483552068"} {:id "defn-/nested-sut-invoke?", :kind "defn-", :line 335, :end-line 343, :hash "-1573228359"} {:id "defn-/link-result", :kind "defn-", :line 345, :end-line 356, :hash "-853542061"} {:id "defn/provenance-link?", :kind "defn", :line 358, :end-line 376, :hash "-2085155288"}]}
;; clj-mutate-manifest-end
