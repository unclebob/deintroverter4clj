(ns deintroverter.forms)

(def ^:private deref-heads #{'deref 'clojure.core/deref})

(defn children
  "Return child nodes of a form tree node."
  [node]
  (cond
    (map? node) (vals node)
    (vector? node) (seq node)
    (seq? node) (seq node)
    :else nil))

(defn- push-children [stack child-nodes]
  (if-let [xs (seq child-nodes)]
    (into stack (reverse (vec xs)))
    stack))

(defn walk-nodes
  "Depth-first preorder walk of all nodes in a form."
  [form]
  (loop [stack [form] nodes []]
    (if (empty? stack)
      nodes
      (let [node (peek stack)
            stack (pop stack)]
        (recur (push-children stack (children node))
               (conj nodes node))))))

(defn- nodes-matching [pred form]
  (into #{} (filter pred (walk-nodes form))))

(defn symbols-in-form
  "All symbols appearing anywhere in form."
  [form]
  (nodes-matching symbol? form))

(defn collect-seq-forms
  "All list/seq subforms in form."
  [form]
  (nodes-matching seq? form))

(defn deref-form?
  "True when form is (deref sym) or (deref (var #'sym))."
  [form]
  (and (seq? form) (= 2 (count form))
       (contains? deref-heads (first form))))

(defn collect-deref-forms
  "All deref subforms in form."
  [form]
  (vec (filter deref-form? (walk-nodes form))))

(defn- deref-var-target-sym [arg]
  (when (and (seq? arg) (= 'var (first arg)) (symbol? (second arg)))
    (second arg)))

(defn deref-target-sym
  "Symbol dereferenced by a deref form, if statically known."
  [form]
  (when (deref-form? form)
    (let [arg (second form)]
      (cond
        (symbol? arg) arg
        :else (deref-var-target-sym arg)))))

(defn deref-target-syms-in-form
  "All statically-known deref targets in form."
  [form]
  (into #{} (keep deref-target-sym (collect-deref-forms form))))

(defn symbols-in-form-outside-deref
  "Symbols in form, excluding symbols nested inside deref forms."
  [form]
  (loop [stack [form] syms #{}]
    (if (empty? stack)
      syms
      (let [node (peek stack)
            stack (pop stack)
            syms (if (symbol? node) (conj syms node) syms)
            child-nodes (if (deref-form? node) [] (or (children node) []))]
        (recur (push-children stack child-nodes) syms)))))

(defn symbol-in-form?
  "True when sym appears anywhere in form."
  [sym form]
  (cond
    (= form sym) true
    (seq? form) (boolean (some #(symbol-in-form? sym %) form))
    :else false))