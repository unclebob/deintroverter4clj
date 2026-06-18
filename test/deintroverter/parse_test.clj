(ns deintroverter.parse-test
  (:require [clojure.test :refer [deftest is]]
            [deintroverter.parse :as parse]))

(def sample-ns-form
  '(ns myapp.core-test
     (:require [clojure.test :refer [deftest is]]
               [myapp.core :as core])))

(deftest reads-forms-with-metadata
  (is (= 2 (count (parse/read-string-all
                   (str "(ns myapp.core-test)\n"
                        "(deftest t (is true))"))))))

(deftest parses-ns-requires-and-aliases
  (is (= 'myapp.core-test
         (:namespace (parse/parse-ns-form sample-ns-form))))
  (is (= 'myapp.core
         (get (:aliases (parse/parse-ns-form sample-ns-form)) 'core)))
  (is (= '#{clojure.test myapp.core}
         (set (:requires (parse/parse-ns-form sample-ns-form))))))