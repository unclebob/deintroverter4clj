(ns deintroverter.assertions)

(def ^:private known
  '{is :is are :are
    should= :should= should== :should== should-not= :should-not=
    should-be :should-be should-not :should-not
    should-not-be :should-not-be
    should-throw? :should-throw? should-not-throw? :should-not-throw?
    should-throw :should-throw should-not-throw :should-not-throw
    should :should
    should-contain :should-contain
    should-not-contain :should-not-contain
    should-fail :should-fail
    should-be-a :should-be-a
    should-be-nil :should-be-nil
    should-not-be-nil :should-not-be-nil
    should> :should>
    should-have-invoked :should-have-invoked
    should-not-have-invoked :should-not-have-invoked})

(defn stub-invocation?
  "True when the parsed assertion checks a Speclj stub invocation."
  [{:keys [macro]}]
  (contains? #{:should-have-invoked :should-not-have-invoked} macro))

(defn- unquote [form]
  (if (and (seq? form) (= 'quote (first form)))
    (second form)
    form))

(defn- asserted-from-is [body]
  (let [form (unquote body)]
    (if (and (seq? form) (= '= (first form)))
      form
      form)))

(defn- asserted-from-should [args]
  (if (= 1 (count args))
    (first args)
    (last args)))

(defn- asserted-from-should-contain [args]
  (if (< 1 (count args)) (second args) (first args)))

(defn- asserted-from-should-be-a [args]
  (first args))

(defn- asserted-from-throw [args]
  (last args))

(defn- two-arg-value [args]
  (if (< 1 (count args)) (second args) (first args)))

(defn- assertion-like? [sym]
  (when (symbol? sym)
    (let [n (name sym)]
      (or (.startsWith n "should")
          (.startsWith n "assert")
          (.startsWith n "expect")))))

(defn- unknown-assertion [mac]
  (when (assertion-like? mac)
    {:macro nil :asserted-form nil :reason :unknown-assertion-macro}))

(defn- parse-known-macro [kw args]
  (let [asserted (get {:is (asserted-from-is (first args))
                       :are (second args)
                       :should= (two-arg-value args)
                       :should== (two-arg-value args)
                       :should-not= (two-arg-value args)
                       :should-not (two-arg-value args)
                       :should> (first args)
                       :should-have-invoked nil
                       :should-not-have-invoked nil
                       :should-be (first args)
                       :should-not-be (first args)
                       :should-be-nil (first args)
                       :should-not-be-nil (first args)
                       :should-throw? (asserted-from-throw args)
                       :should-not-throw? (asserted-from-throw args)
                       :should-throw (asserted-from-throw args)
                       :should-not-throw (asserted-from-throw args)
                       :should (asserted-from-should args)
                       :should-contain (asserted-from-should-contain args)
                       :should-not-contain (asserted-from-should-contain args)
                       :should-fail (first args)
                       :should-be-a (asserted-from-should-be-a args)}
                      kw)]
    {:macro kw :asserted-form asserted :reason nil}))

(defn parse-assertion
  "Returns {:macro keyword|:nil :asserted-form form|:nil :reason keyword|:nil},
  or nil when the form is not an assertion macro."
  [form]
  (when (and (seq? form) (symbol? (first form)))
    (let [mac (first form)
          kw  (get known mac)
          args (rest form)]
      (if kw
        (parse-known-macro kw args)
        (unknown-assertion mac)))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-06-21T09:31:35.234849-05:00", :module-hash "-730697916", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "-1713114668"} {:id "def/known", :kind "def", :line 3, :end-line 19, :hash "-1956720196"} {:id "defn/stub-invocation?", :kind "defn", :line 21, :end-line 24, :hash "-1547926010"} {:id "defn-/unquote", :kind "defn-", :line 26, :end-line 29, :hash "-1954946016"} {:id "defn-/asserted-from-is", :kind "defn-", :line 31, :end-line 35, :hash "1411151168"} {:id "defn-/asserted-from-should", :kind "defn-", :line 37, :end-line 40, :hash "198631767"} {:id "defn-/asserted-from-should-contain", :kind "defn-", :line 42, :end-line 43, :hash "-1728935671"} {:id "defn-/asserted-from-should-be-a", :kind "defn-", :line 45, :end-line 46, :hash "736434591"} {:id "defn-/asserted-from-throw", :kind "defn-", :line 48, :end-line 49, :hash "-739898750"} {:id "defn-/two-arg-value", :kind "defn-", :line 51, :end-line 52, :hash "430071131"} {:id "defn-/assertion-like?", :kind "defn-", :line 54, :end-line 59, :hash "-561846714"} {:id "defn-/unknown-assertion", :kind "defn-", :line 61, :end-line 63, :hash "-1639737051"} {:id "defn-/parse-known-macro", :kind "defn-", :line 65, :end-line 89, :hash "1313739536"} {:id "defn/parse-assertion", :kind "defn", :line 91, :end-line 101, :hash "874611523"}]}
;; clj-mutate-manifest-end
