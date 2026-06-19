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
    (cond
      (and (seq? form) (= '= (first form))) form
      :else form)))

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

(defn- assertion-like? [sym]
  (when (symbol? sym)
    (let [n (name sym)]
      (or (.startsWith n "should")
          (.startsWith n "assert")
          (.startsWith n "expect")))))

(defn parse-assertion
  "Returns {:macro keyword|:nil :asserted-form form|:nil :reason keyword|:nil},
  or nil when the form is not an assertion macro."
  [form]
  (when (and (seq? form) (symbol? (first form)))
    (let [mac (first form)
          kw  (get known mac)
          args (rest form)]
      (cond
        (nil? kw)
        (when (assertion-like? mac)
          {:macro nil :asserted-form nil :reason :unknown-assertion-macro})

        (= :is kw)
        {:macro :is :asserted-form (asserted-from-is (second form)) :reason nil}

        (= :are kw)
        {:macro :are :asserted-form (second form) :reason nil}

        (#{:should= :should== :should-not :should-not=} kw)
        {:macro kw
         :asserted-form (if (< 1 (count args)) (second args) (first args))
         :reason nil}

        (= :should> kw)
        {:macro :should> :asserted-form (first args) :reason nil}

        (stub-invocation? {:macro kw})
        {:macro kw :asserted-form nil :reason nil}

        (#{:should-be :should-not-be :should-be-nil :should-not-be-nil} kw)
        {:macro kw :asserted-form (first args) :reason nil}

        (#{:should-throw? :should-not-throw? :should-throw :should-not-throw} kw)
        {:macro kw :asserted-form (asserted-from-throw args) :reason nil}

        (= :should kw)
        {:macro :should :asserted-form (asserted-from-should args) :reason nil}

        (= :should-contain kw)
        {:macro :should-contain
         :asserted-form (asserted-from-should-contain args)
         :reason nil}

        (= :should-not-contain kw)
        {:macro :should-not-contain
         :asserted-form (asserted-from-should-contain args)
         :reason nil}

        (= :should-fail kw)
        {:macro :should-fail
         :asserted-form (first args)
         :reason nil}

        (= :should-be-a kw)
        {:macro :should-be-a
         :asserted-form (asserted-from-should-be-a args)
         :reason nil}

        :else
        {:macro kw :asserted-form (second form) :reason nil}))))