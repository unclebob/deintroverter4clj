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

(deftest projects-sut-bound-symbol-trace-to-sut
  (let [bindings {'command '(myapp.core/calculate-total items)}]
    (is (= {:verdict :extroverted :reason nil}
           (trace/trace-form '(:total command) bindings trace-ctx)))
    (is (= {:verdict :extroverted :reason nil}
           (trace/trace-form '(str/includes? (:message command) "ok")
                             bindings
                             trace-ctx)))))

(deftest projects-non-sut-bound-symbol-stays-introverted
  (let [bindings {'items '[1 2 3]}]
    (is (= {:verdict :introverted :reason :no-sut-assertion}
           (trace/trace-form '(count items) bindings trace-ctx)))))

(deftest var-invoke-of-sut-fn-is-extroverted
  (is (= {:verdict :extroverted :reason nil}
         (trace/trace-form '((var myapp.core/rebuild-coll) walk nil nil 42)
                           {}
                           trace-ctx))))

(deftest var-form-of-sut-fn-is-extroverted
  (is (= {:verdict :extroverted :reason nil}
         (trace/trace-form '(var myapp.core/rebuild-coll) {} trace-ctx))))