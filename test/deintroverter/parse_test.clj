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

(deftest reads-var-literals
  (is (= 1 (count (parse/read-string-all "(ns t (:require [foo.bar]))"))))
  (is (= 1 (count (parse/read-string-all "(def x #'foo.bar/baz)"))))
  (is (= 1 (count (parse/read-string-all "(def y @atom)")))))

(deftest reads-quote-shorthand-in-map-literals
  (is (= '{:parent (quote (+ 1 2))}
         (first (parse/read-string-all "{:parent '(+ 1 2)}")))))

(deftest parses-ns-requires-and-aliases
  (is (= 'myapp.core-test
         (:namespace (parse/parse-ns-form sample-ns-form))))
  (is (= 'myapp.core
         (get (:aliases (parse/parse-ns-form sample-ns-form)) 'core)))
  (is (= '#{clojure.test myapp.core}
         (set (:requires (parse/parse-ns-form sample-ns-form))))))

(deftest parses-refer-all-and-refer-syms
  (let [ns-form '(ns myapp.wrapper-spec
                  (:require [myapp.wrapper :refer :all]
                            [speclj.core :refer [describe it should=]]))
        parsed  (parse/parse-ns-form ns-form)]
    (is (= '#{myapp.wrapper} (:refer-all parsed)))
    (is (= '{describe speclj.core, it speclj.core, should= speclj.core}
           (:refer-syms parsed)))))

(deftest parses-quoted-symbol-require-entry
  (let [parsed (parse/parse-ns-form '(ns myapp.quoted-spec (:require 'myapp.core)))]
    (is (contains? (:requires parsed) 'myapp.core))))

(deftest parses-ns-without-require-clause
  (let [parsed (parse/parse-ns-form '(ns myapp.bare))]
    (is (= 'myapp.bare (:namespace parsed)))
    (is (empty? (:requires parsed)))))

(deftest reads-source-without-ns-form
  (is (= 1 (count (parse/read-string-all "(def x 1)")))))

(deftest reads-current-namespace-keywords
  (let [forms (parse/read-string-all
               (str "(ns myapp.geometry-spec)\n"
                    "(s/def ::number number?)"))]
    (is (= 2 (count forms)))
    (is (= '(s/def :myapp.geometry-spec/number number?) (second forms)))))

(deftest reads-defmacro-with-syntax-quote
  (is (= 2
         (count (parse/read-string-all
                 "(ns t)\n(defmacro with-log-path [path & body]\n  `(let [x# 1] ~@body))")))))

(deftest reads-cljc-reader-conditionals
  (let [forms (parse/read-string-all
               (str "(ns myapp.format)\n"
                    "(defn format-cell [x] x)\n"
                    "#?(:clj (defn cljc-only [] :clj) :cljs (defn cljc-only [] :cljs))"))]
    (is (= 3 (count forms)))
    (is (= 'myapp.format (:namespace (parse/parse-ns-form (first forms)))))))

(deftest namespace-from-source-reads-reader-conditionals-in-ns-form
  (is (= 'myapp.protocols
         (parse/namespace-from-source
          (str "(ns myapp.protocols\n"
               "  #?(:cljs (:refer-clojure :exclude [clone]))\n"
               "  (:require [clojure.spec.alpha :as s]))\n"
               "(defn update-elements [x] x)"))))
  (is (= 'myapp.core
         (parse/namespace-from-source
          (str "(ns myapp.core\n"
               "  (:require [foo.bar :as q #?@(:cljs [:include-macros true])]\n"
               "            #?(:clj [clojure.java.io :as io])))\n"
               "(defn process-events [x] x)")))))

(deftest reads-alias-qualified-keywords
  (let [forms (parse/read-string-all
               (str "(ns myapp.game-spec\n"
                    "  (:require [clojure.spec.alpha :as s]\n"
                    "            [myapp.game :as game]))\n"
                    "(s/explain-data ::s/player (game/make-player))"))]
    (is (= 2 (count forms)))))