(ns deintroverter.assertions)

(def ^:private known
  '{is :is are :are
    should= :should= should== :should==
    should-be :should-be should-not :should-not
    should-not-be :should-not-be
    should-throw? :should-throw? should-not-throw? :should-not-throw?})

(defn- unquote [form]
  (if (and (seq? form) (= 'quote (first form)))
    (second form)
    form))

(defn- asserted-from-is [body]
  (let [form (unquote body)]
    (cond
      (and (seq? form) (= '= (first form))) form
      :else form)))

(defn- asserted-from-should= [args]
  (first args))

(defn- asserted-from-should-be [args]
  (first args))

(defn parse-assertion
  "Returns {:macro keyword|:nil :asserted-form form|:nil :reason keyword|:nil}"
  [form]
  (when (seq? form)
    (let [mac (first form)
          kw  (get known mac)]
      (cond
        (nil? kw)
        {:macro nil :asserted-form nil :reason :unknown-assertion-macro}

        (= :is kw)
        {:macro :is :asserted-form (asserted-from-is (second form)) :reason nil}

        (= :are kw)
        {:macro :are :asserted-form (second form) :reason nil}

        (#{:should= :should== :should-not} kw)
        (let [args (rest form)]
          {:macro kw
           :asserted-form (if (< 1 (count args)) (second args) (first args))
           :reason nil})

        (#{:should-be :should-not-be} kw)
        {:macro kw :asserted-form (asserted-from-should-be (rest form)) :reason nil}

        (#{:should-throw? :should-not-throw?} kw)
        {:macro kw :asserted-form (second form) :reason nil}

        :else
        {:macro kw :asserted-form (second form) :reason nil}))))