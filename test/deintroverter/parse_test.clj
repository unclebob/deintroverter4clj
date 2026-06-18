(ns deintroverter.parse-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.parse :as parse]))

(def sample-ns-form
  '(ns myapp.core-test
     (:require [clojure.test :refer [deftest is]]
               [myapp.core :as core])))

(deftest reads-forms-with-metadata
  (let [forms (parse/read-string-all
               (str "(ns myapp.core-test)\n"
                    "(deftest t (is true))"))]
    (is (= 2 (count forms)))
    (is (= 'myapp.core-test (second (first forms))))))

(deftest parses-ns-requires-and-aliases
  (let [{:keys [namespace aliases requires]} (parse/parse-ns-form sample-ns-form)]
    (is (= 'myapp.core-test namespace))
    (is (= 'myapp.core (get aliases 'core)))
    (is (= '#{clojure.test myapp.core} (set requires)))))