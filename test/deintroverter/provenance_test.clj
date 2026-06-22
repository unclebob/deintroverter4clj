(ns deintroverter.provenance-test
  (:require [clojure.test :refer [deftest is testing]]
            [deintroverter.provenance :as prov]
            [deintroverter.trace :as trace]))

(def sut #{'myapp.core})
(def resolve-ns (fn [sym] (if (symbol? sym) (name sym) sym)))
(def ns-info {:namespace 'myapp.core-test
              :requires #{'myapp.core}
              :refer-syms {}
              :refer-all #{}})
(def trace-ctx (trace/make-trace-ctx ns-info sut resolve-ns))

(defn- prov-kind [expr bindings & [opts]]
  (:kind (prov/derive-provenance expr bindings trace-ctx opts)))

(deftest literal-provenance-constructor
  (is (= {:kind :literal :source "plain" :confidence :proven :via []}
         (prov/literal-provenance "plain"))))

(deftest sut-invoke-provenance-constructor
  (is (= {:kind :sut-invoke
          :source '(myapp.core/calculate-total items)
          :confidence :proven
          :via []}
         (prov/sut-invoke-provenance '(myapp.core/calculate-total items) :proven))))

(deftest catch-derived-provenance-constructor
  (is (= {:kind :catch-derived :source '(ex-data e) :confidence :proven :via []}
         (prov/catch-derived-provenance '(ex-data e)))))

(deftest fixture-provenance-constructor
  (is (= {:kind :fixture :source 'initial-map :confidence :proven :via []}
         (prov/fixture-provenance 'initial-map))))

(deftest derive-sut-direct-invoke
  (is (= :sut-invoke (prov-kind '(myapp.core/calculate-total items) {})))
  (is (= :proven (:confidence (prov/derive-provenance
                                '(myapp.core/calculate-total items)
                                {}
                                trace-ctx)))))

(deftest derive-sut-refer-all-is-likely
  (let [ctx (trace/make-trace-ctx
             {:refer-syms {} :refer-all #{'myapp.core}}
             sut
             resolve-ns)
        prov (prov/derive-provenance '(calculate-total items) {} ctx)]
    (is (= :sut-invoke (:kind prov)))
    (is (= :likely (:confidence prov)))))

(deftest derive-get-on-sut-result
  (let [bindings {'result '(myapp.core/calculate-total items)}]
    (is (= :sut-derived
           (prov-kind '(get result :total) bindings)))
    (is (= [:get] (:via (prov/derive-provenance '(get result :total) bindings trace-ctx))))))

(deftest derive-get-on-literal-map
  (is (= :unknown (prov-kind '(get {:a 1} :a) {}))))

(deftest derive-destructure-expanded-binding
  (let [bindings {'sut-result '(myapp.core/calculate-total [1])
                  'value '(get sut-result :result)}]
    (is (= :sut-derived (prov-kind 'value bindings)))
    (is (= :sut-derived (prov-kind '(get (myapp.core/calculate-total [1]) :result) bindings)))))

(deftest derive-ex-data-from-catch-exception
  (let [bindings {'e '__catch-exception__}
        prov (prov/derive-provenance '(ex-data e) bindings trace-ctx)]
    (is (= :catch-derived (:kind prov)))
    (is (= '(ex-data e) (:source prov)))))

(deftest derive-ex-data-from-catch-derived-binding
  (let [opts {:binding-provs {'e (prov/catch-derived-provenance 'ex)}}
        prov (prov/derive-provenance '(ex-data e) {} trace-ctx opts)]
    (is (= :catch-derived (:kind prov)))))

(deftest derive-deref-propagates-fixture
  (let [opts {:binding-provs {'initial-map (prov/fixture-provenance 'initial-map)}}
        prov (prov/derive-provenance '(clojure.core/deref initial-map) {} trace-ctx opts)]
    (is (= :fixture (:kind prov)))
    (is (= [:deref] (:via prov)))))

(deftest derive-var-deref-sut-invoke
  (is (= :sut-invoke
         (prov-kind '(clojure.core/deref (var myapp.core/rebuild-coll)) {})))
  (is (= :proven
         (:confidence (prov/derive-provenance
                       '(clojure.core/deref (var myapp.core/rebuild-coll))
                       {}
                       trace-ctx)))))

(deftest derive-helper-invoke-reaching-sut
  (let [bindings {'run-helper '(fn [n] (myapp.core/calculate-total [n]))}]
    (is (= :sut-invoke (prov-kind '(run-helper 3) bindings)))
    (is (= :proven (:confidence (prov/derive-provenance '(run-helper 3) bindings trace-ctx))))))

(deftest derive-symbol-resolves-through-bindings
  (let [bindings {'result '(myapp.core/calculate-total items)
                  'value 'result}]
    (is (= :sut-invoke (prov-kind 'value bindings)))))

(deftest derive-test-module-call
  (let [ctx (trace/make-trace-ctx
             (assoc ns-info :requires #{'myapp.core 'myapp.fixture})
             sut
             resolve-ns
             {:test-modules #{'myapp.fixture}})]
    (is (= :test-module
           (:kind (prov/derive-provenance '(myapp.fixture/make-item) {} ctx))))))

(deftest derive-literal-string
  (is (= :literal (prov-kind "plain" {}))))

(deftest derive-unknown-unresolved-symbol
  (is (= :unknown (prov-kind 'missing-sym {}))))

(deftest derivation-depth-cap
  (let [at-cap {:kind :sut-derived :source 'x :confidence :proven :via [:get :get :get]}
        opts {:binding-provs {'x at-cap}}]
    (is (= :unknown
           (:kind (prov/derive-provenance '(get x :k) {} trace-ctx opts))))))

(deftest merge-provenance-prefers-sut-invoke
  (let [merged (prov/merge-provenance
                (prov/literal-provenance "x")
                (prov/sut-invoke-provenance '(myapp.core/foo) :proven))]
    (is (= :sut-invoke (:kind merged)))
    (is (= :proven (:confidence merged)))))

(deftest merge-provenance-concatenates-via
  (let [a {:kind :sut-derived :source 'a :confidence :proven :via [:get]}
        b {:kind :literal :source 'b :confidence :proven :via [:deref]}
        merged (prov/merge-provenance a b)]
    (is (= [:get :deref] (:via merged)))))

(deftest provenance-kind-accessor
  (is (= :literal (prov/provenance-kind (prov/literal-provenance 1)))))

(deftest linkable-kind-predicate
  (is (prov/linkable-kind? :sut-invoke))
  (is (prov/linkable-kind? :sut-derived))
  (is (prov/linkable-kind? :catch-derived))
  (is (not (prov/linkable-kind? :literal)))
  (is (not (prov/linkable-kind? :fixture))))

(deftest legacy-evidence-tag-mapping
  (is (= :sut-result-read
         (prov/legacy-evidence-tag {:kind :sut-derived})))
  (is (= :exception-catch-assertion
         (prov/legacy-evidence-tag {:kind :catch-derived})))
  (is (= :sut-invoke
         (prov/legacy-evidence-tag {:kind :sut-invoke})))
  (is (= :nested-sut-invoke
         (prov/legacy-evidence-tag {:kind :sut-invoke} {:nested? true}))))

(deftest resolve-bound-form-chases-chain
  (let [bindings {'a 'b 'b 'c 'c '(myapp.core/calculate-total items)}]
    (is (= '(myapp.core/calculate-total items)
           (trace/resolve-bound-form 'a bindings)))))

(deftest provenance-link-sut-derived-symbol
  (let [bindings {'value '(get result :total)
                  'result '(myapp.core/calculate-total items)}
        opts {:binding-provs {'value {:kind :sut-derived
                                       :source '(get result :total)
                                       :confidence :proven
                                       :via [:get]}}}
        link (prov/provenance-link? 'value bindings trace-ctx opts)]
    (is (:linked? link))
    (is (= :sut-derived (:kind link)))
    (is (= :sut-result-read (:legacy-evidence link)))))

(deftest provenance-link-literal-no-link
  (is (nil? (prov/provenance-link? '"plain" {} trace-ctx))))

(deftest provenance-link-catch-derived-with-seen-sut
  (let [opts {:binding-provs {'data (prov/catch-derived-provenance '(ex-data e))}
              :walk-state {:seen-sut? true}
              :require-seen-sut? true}]
    (is (= :catch-derived
           (:kind (prov/provenance-link? 'data {} trace-ctx opts))))
    (is (= :exception-catch-assertion
           (:legacy-evidence (prov/provenance-link? 'data {} trace-ctx opts))))))

(deftest provenance-link-catch-derived-without-seen-sut
  (let [opts {:binding-provs {'data (prov/catch-derived-provenance '(ex-data e))}
              :walk-state {:seen-sut? false}
              :require-seen-sut? true}]
    (is (nil? (prov/provenance-link? 'data {} trace-ctx opts)))))

(deftest provenance-link-nested-sut-invoke
  (let [sut* (conj sut 'myapp.debug)
        ctx (trace/make-trace-ctx
             (assoc ns-info :requires #{'myapp.core 'myapp.debug})
             sut*
             resolve-ns)
        asserted '(re-matches #"pos" (myapp.debug/format-cell "pos" "cell"))
        link (prov/provenance-link? asserted {} ctx)]
    (is (:linked? link))
    (is (= :sut-invoke (:kind link)))
    (is (= :nested-sut-invoke (:legacy-evidence link)))))

(deftest provenance-link-direct-sut-invoke
  (let [asserted '(myapp.core/calculate-total items)
        link (prov/provenance-link? asserted {} trace-ctx)]
    (is (:linked? link))
    (is (= :sut-invoke (:kind link)))
    (is (= :sut-invoke (:legacy-evidence link)))))

(deftest provenance-link-helper-destructure-scenario
  (testing "get on helper result field matches fixture pattern"
    (let [bindings {'run-result '((fn [n] {:result (myapp.core/calculate-total [n])}) 3)
                    'value '(get run-result :result)}
          opts {:binding-provs {'value (prov/derive-provenance '(get run-result :result)
                                                               bindings
                                                               trace-ctx)}}
          link (prov/provenance-link? 'value bindings trace-ctx opts)]
      (is (= :sut-derived (:kind link)))
      (is (= :sut-result-read (:legacy-evidence link))))))