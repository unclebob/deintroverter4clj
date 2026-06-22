(ns deintroverter.heuristics.external
  (:require [clojure.set :refer [intersection]]
            [deintroverter.forms :as forms]
            [deintroverter.provenance :as prov]
            [deintroverter.reasons :as reasons]
            [deintroverter.trace :as trace]))

(defn- introverted-asserted?
  [asserted-form bindings trace-ctx]
  (= :introverted (:verdict (trace/trace-form asserted-form bindings trace-ctx))))

(defn- form-reaches-sut?
  [form bindings trace-ctx]
  (or (trace/reaches-sut-likely? form bindings trace-ctx)
      (= :sut-invoke (:kind (prov/derive-provenance form bindings trace-ctx)))))

(defn- fn-form? [form]
  (and (seq? form) (#{'fn 'fn*} (first form))))

(defn- atom-constructor-form? [form]
  (and (seq? form) (= 'atom (first form))))

(defn- atom-bound-sym? [bindings sym]
  (atom-constructor-form? (get bindings sym)))

(defn- let-bound-atom-syms [bindings]
  (into #{} (keep (fn [[k v]]
                    (when (and (symbol? k) (atom-constructor-form? v))
                      k))
                  bindings)))

(declare atom-syms-written-in-form)

(defn- atom-mutation-target-sym [form]
  (when (and (seq? form) (<= 2 (count form)))
    (let [head (first form) target (second form)]
      (when (#{'reset! 'swap! 'clojure.core/reset! 'clojure.core/swap!} head)
        (or (forms/deref-target-sym target)
            (when (symbol? target) target))))))

(defn- atom-mutation-syms-in-seq [form]
  (into (or (when-let [sym (atom-mutation-target-sym form)] #{sym}) #{})
        (mapcat atom-syms-written-in-form form)))

(defn atom-syms-written-in-form
  "Return atom symbols mutated anywhere in form."
  [form]
  (cond
    (nil? form) #{}
    (map? form) (into #{} (mapcat atom-syms-written-in-form (vals form)))
    (vector? form) (into #{} (mapcat atom-syms-written-in-form form))
    (seq? form) (atom-mutation-syms-in-seq form)
    :else #{}))

(defn stub-capture-atoms-from-redefs
  [redefs-bindings]
  (when (vector? redefs-bindings)
    (reduce (fn [acc [_ stub-fn]]
              (if (fn-form? stub-fn)
                (into acc (atom-syms-written-in-form stub-fn))
                acc))
            #{}
            (partition 2 redefs-bindings))))

(defn spy-atoms-written-in-form
  [form bindings]
  (set (filter #(atom-bound-sym? bindings %)
               (atom-syms-written-in-form form))))

(defn sut-call-mutation-atoms
  [form bindings]
  (let [atom-syms (let-bound-atom-syms bindings)]
    (into (intersection atom-syms (forms/symbols-in-form-outside-deref form))
          (mapcat (fn [sym]
                    (when (contains? bindings sym)
                      (intersection atom-syms
                                    (forms/symbols-in-form (get bindings sym)))))
                  (forms/symbols-in-form form)))))

(defn wiring-sut-call
  [walk-state bindings trace-ctx]
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
        (forms/deref-target-syms-in-form asserted-form)))

(defn- sut-atom-deref-target? [sym bindings trace-ctx]
  (and (symbol? sym)
       (not (contains? bindings sym))
       (not (atom-bound-sym? bindings sym))
       (trace/reaches-sut? (list 'clojure.core/deref sym) bindings trace-ctx)))

(defn- sut-atom-read-evidence [asserted-form bindings trace-ctx walk-state]
  (when (introverted-asserted? asserted-form bindings trace-ctx)
    (when (and (:seen-sut? walk-state)
               (wiring-sut-call walk-state bindings trace-ctx))
      (when (some #(sut-atom-deref-target? % bindings trace-ctx)
                  (forms/deref-target-syms-in-form asserted-form))
        :sut-atom-read))))

(defn- world-atom-readback-evidence [asserted-form bindings trace-ctx walk-state]
  (when (introverted-asserted? asserted-form bindings trace-ctx)
    (when (and (:seen-sut? walk-state)
               (wiring-sut-call walk-state bindings trace-ctx)
               (seq (:sut-mutation-atoms walk-state)))
      (when (some #(and (atom-bound-sym? bindings %)
                        (contains? (:sut-mutation-atoms walk-state) %))
                  (forms/deref-target-syms-in-form asserted-form))
        :world-atom-readback))))

(defn- invoke-form? [form]
  (and (seq? form) (symbol? (first form))))

(defn- test-module-state-key-read-form? [form trace-ctx]
  (and (invoke-form? form)
       (trace/reaches-test-module? form {} trace-ctx)
       (pos? (count (rest form)))
       (every? keyword? (rest form))))

(defn- asserted-test-module-state-key-read? [asserted-form trace-ctx]
  (boolean
   (some #(test-module-state-key-read-form? % trace-ctx)
         (forms/collect-seq-forms asserted-form))))

(defn- immediate-preceding-sut-evidence [asserted-form bindings trace-ctx walk-state]
  (when (introverted-asserted? asserted-form bindings trace-ctx)
    (when (and (:preceding walk-state)
               (form-reaches-sut? (:preceding walk-state) bindings trace-ctx)
               (asserted-test-module-state-key-read? asserted-form trace-ctx))
      :immediate-preceding-sut)))

(defn- sut-then-test-module-read-evidence [asserted-form bindings trace-ctx walk-state]
  (when (introverted-asserted? asserted-form bindings trace-ctx)
    (when (and (:seen-sut? walk-state)
               (wiring-sut-call walk-state bindings trace-ctx)
               (asserted-test-module-state-key-read? asserted-form trace-ctx)
               (not (and (:preceding walk-state)
                         (form-reaches-sut? (:preceding walk-state) bindings trace-ctx))))
      :sut-then-test-module-read)))

(defn- test-state-binding-evidence [asserted-form bindings trace-ctx walk-state]
  (when (introverted-asserted? asserted-form bindings trace-ctx)
    (when (and (:seen-sut? walk-state)
               (trace/binding-from-test-module? bindings trace-ctx))
      :test-state-binding)))

(def ^:private file-interop-methods
  #{".exists" ".isDirectory" ".isFile" ".listFiles" ".startsWith" ".getPath" ".getName"})

(defn- interop-method-sym? [sym]
  (and (symbol? sym) (.startsWith (name sym) ".")))

(defn- slurp-form? [form]
  (and (seq? form) (= 'slurp (first form))))

(defn- file-constructor-form? [form]
  (and (seq? form) (#{'File. 'java.io.File} (first form))))

(defn- file-interop-form? [form]
  (and (seq? form) (interop-method-sym? (first form))
       (contains? file-interop-methods (name (first form)))))

(defn- files-invoke? [form]
  (and (seq? form) (symbol? (first form))
       (when-let [ns (namespace (first form))]
         (= "Files" (name ns)))))

(defn- file-dependency-leaf? [form]
  (or (slurp-form? form)
      (file-constructor-form? form)
      (file-interop-form? form)
      (files-invoke? form)))

(defn- file-dependency-form? [form]
  (cond
    (nil? form) false
    (file-dependency-leaf? form) true
    (seq? form) (boolean (some file-dependency-form? form))
    :else false))

(defn- file-dependency-evidence [asserted-form bindings trace-ctx walk-state]
  (when (introverted-asserted? asserted-form bindings trace-ctx)
    (when (and (:seen-sut? walk-state)
               (wiring-sut-call walk-state bindings trace-ctx)
               (file-dependency-form? asserted-form))
      :file-dependency)))

(defn- wiring-capture-evidence [asserted-form bindings trace-ctx walk-state]
  (when (introverted-asserted? asserted-form bindings trace-ctx)
    (when (and (:seen-sut? walk-state)
               (seq (:stub-capture-atoms walk-state))
               (wiring-sut-call walk-state bindings trace-ctx)
               (not (sut-atom-read-evidence asserted-form bindings trace-ctx walk-state)))
      (when (wiring-spy-atom-sym asserted-form bindings walk-state)
        :stub-capture))))

(defn external-evidence
  "Return the first external heuristic evidence tag for an introverted assertion."
  [asserted-form bindings trace-ctx walk-state]
  (or (wiring-capture-evidence asserted-form bindings trace-ctx walk-state)
      (sut-atom-read-evidence asserted-form bindings trace-ctx walk-state)
      (file-dependency-evidence asserted-form bindings trace-ctx walk-state)
      (world-atom-readback-evidence asserted-form bindings trace-ctx walk-state)
      (immediate-preceding-sut-evidence asserted-form bindings trace-ctx walk-state)
      (sut-then-test-module-read-evidence asserted-form bindings trace-ctx walk-state)
      (test-state-binding-evidence asserted-form bindings trace-ctx walk-state)))

(defn- likely-extroverted-result-map
  [assertion-form bindings trace-ctx walk-state
   {:keys [reason reason-legacy trace-assoc trace-target preceding-sut-call?] :as opts}]
  (let [sut-call (wiring-sut-call walk-state bindings trace-ctx)
        trace-target (or trace-target sut-call assertion-form)]
    {:verdict :likely-extroverted
     :reason reason
     :reason-legacy reason-legacy
     :trace (cond-> (trace/explain-trace trace-target bindings trace-ctx)
              true (assoc :assertion-form assertion-form)
              true (merge trace-assoc)
              preceding-sut-call? (assoc :preceding-sut-call sut-call))}))

(defn- stub-capture-result [assertion-form bindings trace-ctx walk-state]
  (let [{:keys [reason reason-legacy]} (reasons/external-wiring-result)]
    (likely-extroverted-result-map assertion-form bindings trace-ctx walk-state
                                   {:reason reason
                                    :reason-legacy reason-legacy
                                    :trace-assoc {:wiring-evidence :stub-capture}
                                    :preceding-sut-call? true})))

(defn- file-dependency-result [assertion-form bindings trace-ctx walk-state]
  (let [{:keys [reason reason-legacy]} (reasons/external-file-result)]
    (likely-extroverted-result-map assertion-form bindings trace-ctx walk-state
                                   {:reason reason
                                    :reason-legacy reason-legacy
                                    :trace-assoc {:external-dependency-evidence :file-dependency}
                                    :preceding-sut-call? true})))

(defn- side-effect-result
  [assertion-form bindings trace-ctx walk-state evidence & [opts]]
  (let [{:keys [reason reason-legacy]} (reasons/provenance-link-result
                                         :sut-side-effect-heuristic)]
    (likely-extroverted-result-map assertion-form bindings trace-ctx walk-state
                                   (merge {:reason reason
                                           :reason-legacy reason-legacy
                                           :trace-assoc {:side-effect-evidence evidence}
                                           :preceding-sut-call? true}
                                          opts))))

(def ^:private evidence-result-builders
  {:stub-capture (fn [af _ b tc ws _] (stub-capture-result af b tc ws))
   :file-dependency (fn [af _ b tc ws _] (file-dependency-result af b tc ws))
   :sut-atom-read (fn [af _ b tc ws e] (side-effect-result af b tc ws e))
   :world-atom-readback (fn [af _ b tc ws e] (side-effect-result af b tc ws e))
   :immediate-preceding-sut
   (fn [af _ b tc ws e]
     (side-effect-result af b tc ws e {:trace-target (:preceding ws)}))
   :sut-then-test-module-read
   (fn [af _ b tc ws e]
     (side-effect-result af b tc ws e))
   :test-state-binding
   (fn [af afm b tc ws e]
     (side-effect-result af b tc ws e {:trace-target afm :preceding-sut-call? false}))})

(defn external-assertion-result-map
  [assertion-form asserted-form bindings trace-ctx walk-state evidence]
  ((get evidence-result-builders evidence)
   assertion-form asserted-form bindings trace-ctx walk-state evidence))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-22T09:17:53.740994-05:00", :module-hash "256989905", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 6, :hash "-475029705"} {:id "defn-/introverted-asserted?", :kind "defn-", :line 8, :end-line 10, :hash "-631931369"} {:id "defn-/form-reaches-sut?", :kind "defn-", :line 12, :end-line 15, :hash "1416984373"} {:id "defn-/fn-form?", :kind "defn-", :line 17, :end-line 18, :hash "1534401844"} {:id "defn-/atom-constructor-form?", :kind "defn-", :line 20, :end-line 21, :hash "-422734210"} {:id "defn-/atom-bound-sym?", :kind "defn-", :line 23, :end-line 24, :hash "-760657684"} {:id "defn-/let-bound-atom-syms", :kind "defn-", :line 26, :end-line 30, :hash "988367712"} {:id "form/7/declare", :kind "declare", :line 32, :end-line 32, :hash "1128108022"} {:id "defn-/atom-mutation-target-sym", :kind "defn-", :line 34, :end-line 39, :hash "-106351143"} {:id "defn-/atom-mutation-syms-in-seq", :kind "defn-", :line 41, :end-line 43, :hash "523183641"} {:id "defn/atom-syms-written-in-form", :kind "defn", :line 45, :end-line 53, :hash "647408832"} {:id "defn/stub-capture-atoms-from-redefs", :kind "defn", :line 55, :end-line 63, :hash "-1504350403"} {:id "defn/spy-atoms-written-in-form", :kind "defn", :line 65, :end-line 68, :hash "-1675608979"} {:id "defn/sut-call-mutation-atoms", :kind "defn", :line 70, :end-line 78, :hash "-210585478"} {:id "defn/wiring-sut-call", :kind "defn", :line 80, :end-line 90, :hash "-1034137674"} {:id "defn-/wiring-spy-atom-sym", :kind "defn-", :line 92, :end-line 97, :hash "-553420478"} {:id "defn-/sut-atom-deref-target?", :kind "defn-", :line 99, :end-line 103, :hash "314168095"} {:id "defn-/sut-atom-read-evidence", :kind "defn-", :line 105, :end-line 111, :hash "-1012032693"} {:id "defn-/world-atom-readback-evidence", :kind "defn-", :line 113, :end-line 121, :hash "-597407263"} {:id "defn-/immediate-preceding-sut-evidence", :kind "defn-", :line 123, :end-line 128, :hash "-1335850440"} {:id "defn-/test-state-binding-evidence", :kind "defn-", :line 130, :end-line 134, :hash "-1069145250"} {:id "def/file-interop-methods", :kind "def", :line 136, :end-line 137, :hash "380778420"} {:id "defn-/interop-method-sym?", :kind "defn-", :line 139, :end-line 140, :hash "815540966"} {:id "defn-/slurp-form?", :kind "defn-", :line 142, :end-line 143, :hash "451280593"} {:id "defn-/file-constructor-form?", :kind "defn-", :line 145, :end-line 146, :hash "-28056676"} {:id "defn-/file-interop-form?", :kind "defn-", :line 148, :end-line 150, :hash "-1263170186"} {:id "defn-/files-invoke?", :kind "defn-", :line 152, :end-line 155, :hash "-1633387529"} {:id "defn-/file-dependency-leaf?", :kind "defn-", :line 157, :end-line 161, :hash "-696616296"} {:id "defn-/file-dependency-form?", :kind "defn-", :line 163, :end-line 168, :hash "-1671609403"} {:id "defn-/file-dependency-evidence", :kind "defn-", :line 170, :end-line 175, :hash "-220580323"} {:id "defn-/wiring-capture-evidence", :kind "defn-", :line 177, :end-line 184, :hash "1179225183"} {:id "defn/external-evidence", :kind "defn", :line 186, :end-line 194, :hash "1236499253"} {:id "defn-/likely-extroverted-result-map", :kind "defn-", :line 196, :end-line 207, :hash "693255209"} {:id "defn-/stub-capture-result", :kind "defn-", :line 209, :end-line 215, :hash "2114494137"} {:id "defn-/file-dependency-result", :kind "defn-", :line 217, :end-line 223, :hash "-1241298315"} {:id "defn-/side-effect-result", :kind "defn-", :line 225, :end-line 234, :hash "-807988287"} {:id "def/evidence-result-builders", :kind "def", :line 236, :end-line 246, :hash "-2043192720"} {:id "defn/external-assertion-result-map", :kind "defn", :line 248, :end-line 251, :hash "-1738890083"}]}
;; clj-mutate-manifest-end
