(ns deintroverter.forms-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.forms :as forms]))

(deftest symbol-in-form-detects-nested-occurrence
  (is (forms/symbol-in-form? 'rows '(should= (count rows) 2)))
  (is (not (forms/symbol-in-form? 'missing '(should= (count rows) 2)))))

(deftest symbols-in-form-collects-all-symbols
  (is (= #{'f 'x 'g 'y} (forms/symbols-in-form '(f x (g y))))))

(deftest collect-seq-forms-finds-nested-lists
  (is (contains? (forms/collect-seq-forms '(should= (count rows) 2))
                 '(count rows))))

(deftest deref-target-sym-resolves-symbol-and-var
  (is (= 'state (forms/deref-target-sym '(deref state))))
  (is (= 'myapp/status
         (forms/deref-target-sym '(deref (var myapp/status))))))

(deftest deref-target-syms-in-form-collects-targets
  (is (= #{'a 'b}
         (forms/deref-target-syms-in-form '(f (deref a) (deref b))))))

(deftest symbols-in-form-outside-deref-skips-deref-children
  (is (= #{'f 'outer}
         (forms/symbols-in-form-outside-deref '(f outer (deref inner))))))

(deftest collect-deref-forms-finds-deref-subforms
  (is (= ['(deref a)]
         (forms/collect-deref-forms '(f (deref a) 1)))))