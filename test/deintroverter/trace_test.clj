(ns deintroverter.trace-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.trace :as trace]))

(def sut #{'myapp.core})
(def resolve-ns (fn [sym] (if (symbol? sym) (name sym) sym)))
(def ns-info {:refer-syms {} :refer-all #{}})
(def trace-ctx (trace/make-trace-ctx ns-info sut resolve-ns))

(deftest direct-sut-call-is-extroverted
  (is (= {:verdict :extroverted :reason nil}
         (trace/trace-form '(myapp.core/calculate-total items)
                           {}
                           trace-ctx))))

(deftest let-binding-to-sut-call-is-extroverted
  (is (= {:verdict :extroverted :reason nil}
         (trace/trace-form 'result
                           (array-map 'result '(myapp.core/calculate-total items))
                           trace-ctx))))

(deftest thread-first-desugars-to-sut
  (is (= {:verdict :extroverted :reason nil}
         (trace/trace-form
          '(-> items (myapp.core/calculate-total) (myapp.core/format))
          {}
          trace-ctx))))

(deftest non-sut-only-is-introverted
  (is (= {:verdict :introverted :reason :no-sut-assertion}
         (trace/trace-form '(count items) {} trace-ctx))))

(deftest destructuring-is-questionable
  (is (= {:verdict :questionable :reason :destructuring}
         (trace/trace-form 'x
                           (array-map :destructuring? true)
                           trace-ctx))))

(deftest refer-all-unqualified-call-is-likely-extroverted
  (let [ctx (trace/make-trace-ctx
             {:refer-syms {} :refer-all #{'myapp.core}}
             sut
             resolve-ns)]
    (is (= {:verdict :likely-extroverted :reason :refer-all-heuristic}
           (trace/trace-form '(calculate-total items) {} ctx)))))

(deftest refer-syms-unqualified-call-is-extroverted
  (let [ctx (trace/make-trace-ctx
             {:refer-syms {'calculate-total 'myapp.core} :refer-all #{}}
             sut
             resolve-ns)]
    (is (= {:verdict :extroverted :reason nil}
           (trace/trace-form '(calculate-total items) {} ctx)))))

(deftest core-sym-via-refer-all-stays-introverted
  (let [ctx (trace/make-trace-ctx
             {:refer-syms {} :refer-all #{'myapp.core}}
             sut
             resolve-ns)]
    (is (= {:verdict :introverted :reason :no-sut-assertion}
           (trace/trace-form '(count items) {} ctx)))))