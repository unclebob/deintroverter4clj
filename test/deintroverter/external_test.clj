(ns deintroverter.external-test
  (:require [clojure.test :refer [deftest is testing]]
            [deintroverter.heuristics.external :as external]
            [deintroverter.trace :as trace]))

(def sut #{'myapp.core})
(def ns-info {:namespace 'myapp.external-test
              :requires #{'myapp.core 'myapp.helpers-test}
              :aliases {'helpers 'myapp.helpers-test}
              :refer-syms {}
              :refer-all #{}})
(def resolve-ns (fn [sym-or-alias]
                  (when (symbol? sym-or-alias)
                    (if-let [ns-part (namespace sym-or-alias)]
                      (if-let [resolved (get (:aliases ns-info) (symbol ns-part))]
                        (name resolved)
                        ns-part)
                      (if-let [resolved (get (:aliases ns-info) sym-or-alias)]
                        (name resolved)
                        (name sym-or-alias))))))
(def test-modules #{'myapp.helpers-test})
(def trace-ctx (trace/make-trace-ctx ns-info sut resolve-ns {:test-modules test-modules}))

(defn- walk-with-sut [forms]
  {:seen-sut? true
   :last-sut-call '(myapp.core/run)
   :preceding '(myapp.core/run)
   :stub-capture-atoms #{'spy}
   :sut-mutation-atoms #{'world}})

(deftest stub-capture-atoms-from-redefs-finds-written-atoms
  (is (= #{'captured}
         (external/stub-capture-atoms-from-redefs
          ['stub (list 'fn [] '(reset! captured 1))]))))

(deftest spy-atoms-written-in-form-finds-let-bound-atoms
  (let [bindings {'spy '(atom nil)}]
    (is (= #{'spy}
           (external/spy-atoms-written-in-form '(reset! spy 1) bindings)))))

(deftest external-evidence-detects-file-dependency
  (let [bindings {}
        walk-state (walk-with-sut ['(myapp.core/run)])
        asserted '(File. "tmp.txt")]
    (is (= :file-dependency
           (external/external-evidence asserted bindings trace-ctx walk-state)))))

(deftest external-evidence-detects-stub-capture
  (let [bindings {'spy '(atom nil)}
        walk-state (assoc (walk-with-sut nil) :stub-capture-atoms #{'spy})
        asserted '(should= @spy 1)]
    (is (= :stub-capture
           (external/external-evidence asserted bindings trace-ctx walk-state)))))

(deftest external-assertion-result-map-normalizes-reasons
  (let [bindings {'spy '(atom nil)}
        walk-state (assoc (walk-with-sut nil) :stub-capture-atoms #{'spy})
        result (external/external-assertion-result-map
                '(should= @spy 1) '(should= @spy 1) bindings trace-ctx walk-state
                :stub-capture)]
    (is (= :external-wiring (:reason result)))
    (is (= :sut-wiring-heuristic (:reason-legacy result)))))

(deftest external-evidence-detects-deferred-sut-test-module-read
  (let [bindings {}
        walk-state (assoc (walk-with-sut nil)
                          :preceding '(helpers/set-test-world! {})
                          :last-sut-call '(myapp.core/run))
        asserted '(get-in (helpers/read-test-state :game-map) [0 0])]
    (is (= :sut-then-test-module-read
           (external/external-evidence asserted bindings trace-ctx walk-state)))))

(deftest external-evidence-skips-deferred-when-immediate-preceding-sut
  (let [bindings {}
        walk-state (walk-with-sut ['(myapp.core/run)])
        asserted '(helpers/read-test-state :game-map)]
    (is (= :immediate-preceding-sut
           (external/external-evidence asserted bindings trace-ctx walk-state)))))

(deftest external-evidence-skips-predicate-helper-after-sut
  (let [bindings {}
        walk-state (assoc (walk-with-sut nil) :preceding '(setup 1))
        asserted '(helpers/valid-input? 1)]
    (is (nil? (external/external-evidence asserted bindings trace-ctx walk-state)))))

(deftest external-assertion-result-map-covers-remaining-evidence-tags
  (let [bindings {'world '(atom {})}
        walk-state (assoc (walk-with-sut nil) :sut-mutation-atoms #{'world})
        file-result (external/external-assertion-result-map
                     '(should= 1 1) '(File. "x") bindings trace-ctx walk-state
                     :file-dependency)
        world-result (external/external-assertion-result-map
                      '(should= @world :ok) '(should= @world :ok)
                      bindings trace-ctx walk-state :world-atom-readback)
        preceding-result (external/external-assertion-result-map
                          '(should= {} (helpers/read-test-state :game-map))
                          '(helpers/read-test-state :game-map)
                          bindings trace-ctx walk-state
                          :immediate-preceding-sut)
        binding-result (external/external-assertion-result-map
                        '(should= state 1) 'state bindings trace-ctx walk-state
                        :test-state-binding)]
    (is (= :external-file (:reason file-result)))
    (is (= :provenance-link (:reason world-result)))
    (is (= :provenance-link (:reason preceding-result)))
    (is (= :provenance-link (:reason binding-result)))))