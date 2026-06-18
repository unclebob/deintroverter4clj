(ns deintroverter.assertions)

(def ^:private known
  '{is :is are :are
    should= :should= should== :should==
    should-be :should-be should-not :should-not
    should-not-be :should-not-be
    should-throw? :should-throw? should-not-throw? :should-not-throw?
    should :should
    should-contain :should-contain
    should-be-a :should-be-a})

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

(defn parse-assertion
  "Returns {:macro keyword|:nil :asserted-form form|:nil :reason keyword|:nil}"
  [form]
  (when (seq? form)
    (let [mac (first form)
          kw  (get known mac)
          args (rest form)]
      (cond
        (nil? kw)
        {:macro nil :asserted-form nil :reason :unknown-assertion-macro}

        (= :is kw)
        {:macro :is :asserted-form (asserted-from-is (second form)) :reason nil}

        (= :are kw)
        {:macro :are :asserted-form (second form) :reason nil}

        (#{:should= :should== :should-not} kw)
        {:macro kw
         :asserted-form (if (< 1 (count args)) (second args) (first args))
         :reason nil}

        (#{:should-be :should-not-be} kw)
        {:macro kw :asserted-form (first args) :reason nil}

        (#{:should-throw? :should-not-throw?} kw)
        {:macro kw :asserted-form (second form) :reason nil}

        (= :should kw)
        {:macro :should :asserted-form (asserted-from-should args) :reason nil}

        (= :should-contain kw)
        {:macro :should-contain
         :asserted-form (asserted-from-should-contain args)
         :reason nil}

        (= :should-be-a kw)
        {:macro :should-be-a
         :asserted-form (asserted-from-should-be-a args)
         :reason nil}

        :else
        {:macro kw :asserted-form (second form) :reason nil}))))