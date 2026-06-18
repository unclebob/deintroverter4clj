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

(deftest collects-clojure-files-recursively
  (let [dir (tmp-dir)]
    (try
      (write-file dir "src/a.clj" "(ns a)")
      (write-file dir "src/sub/b.cljc" "(ns b)")
      (write-file dir "src/sub/skip.txt" "nope")
      (write-file dir "nested/deep/c.cljs" "(ns c)")
      (is (= #{"a.clj" "b.cljc" "c.cljs"}
             (set (map #(.getName %) (paths/collect-files [(.getPath dir)])))))
      (finally
        (.delete (io/file dir "src/sub/b.cljc"))
        (.delete (io/file dir "src/sub"))
        (.delete (io/file dir "src/a.clj"))
        (.delete (io/file dir "src"))
        (.delete (io/file dir "nested/deep/c.cljs"))
        (.delete (io/file dir "nested/deep"))
        (.delete (io/file dir "nested"))
        (.delete dir)))))

(deftest accepts-single-file
  (let [dir (tmp-dir)
        f   (write-file dir "one.clj" "(ns one)")]
    (try
      (is (= [f] (paths/collect-files [(.getPath f)])))
      (finally (.delete f) (.delete dir)))))