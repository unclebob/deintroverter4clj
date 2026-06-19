(ns deintroverter.paths-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [deintroverter.paths :as paths]))

(defn- tmp-dir []
  (doto (io/file (System/getProperty "java.io.tmpdir")
                  (str "deintroverter-" (random-uuid)))
    .mkdirs))

(defn- write-file [dir name content]
  (let [f (io/file dir name)]
    (.mkdirs (.getParentFile f))
    (spit f content)
    f))

(defn- delete-tree [^java.io.File f]
  (when (.exists f)
    (doseq [child (.listFiles f)] (delete-tree child))
    (.delete f)))

(defn- file-names [files]
  (set (map #(.getName ^java.io.File %) files)))

(deftest collects-clojure-files-recursively
  (let [dir (tmp-dir)]
    (try
      (write-file dir "src/a.clj" "(ns a)")
      (write-file dir "src/sub/b.cljc" "(ns b)")
      (write-file dir "src/sub/skip.txt" "nope")
      (write-file dir "nested/deep/c.cljs" "(ns c)")
      (is (= #{"a.clj" "b.cljc" "c.cljs"}
             (file-names (paths/collect-files [(.getPath dir)]))))
      (finally (delete-tree dir)))))

(deftest accepts-single-file
  (let [dir (tmp-dir)]
    (try
      (let [f (write-file dir "one.clj" "(ns one)")]
        (is (= [f] (paths/collect-files [(.getPath f)]))))
      (finally (delete-tree dir)))))
