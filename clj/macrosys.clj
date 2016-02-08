(ns centring.macrosys
  (:require [clojure.walk :refer [postwalk]]))

(defn assoc-meta [o k v]
  (vary-meta o assoc k v))

(defn dissoc-meta [o & ks]
  (apply vary-meta o dissoc ks))

;;;

(defn assoc-from-callee [o]
  (if (symbol? o)
    (assoc-meta o :from-callee true)
    o))

(defn binds [form]
  (if (and (seq? form) (seq? (first form))
           (= (ffirst form) 'centring.ct/fn))
    (second (first form))
    '()))

(defn rename [binds o]
  (if (and (symbol? o) (not (:injected (meta o))))
    (get binds o o)
    o))

(defn rename-binds [form]
  (let [bind-names (binds form)]
    (if (not (empty? bind-names))
      (cons (postwalk (partial rename (zipmap bind-names
                                              (map gensym bind-names)))
                      (first form))
            (rest form))
      form)))

;; Clojure's quasiquote behaviour clashes with this (try +-expander!)
;; Also, if the identifier is actually referred, should use its actual ns.
(defn resolve-mns [mns-name form]
  (if (and (symbol? form)
           (not (or (:from-callee (meta form)) (:injected (meta form))))
           (not (namespace form)))
    (symbol mns-name (name form))
    form))

(defn cleanse [form]
  (if (meta form)
    (dissoc-meta form :from-callee :injected)
    form))

(defn inject [sym]
  (assoc-meta sym :injected true))

;;;

;; TODO: deal with quote and quasiquote in input
;; could also combine passes to increase performance
(defn expand-1 [macrodef-ns-name expander form]
  (let [t-form (postwalk assoc-from-callee (rest form))
        et-form (apply expander t-form)
        ret-form (postwalk rename-binds et-form)
        rret-form (postwalk (partial resolve-mns macrodef-ns-name) ret-form)]
    (postwalk cleanse rret-form)))

;;;

(defn let-expander [bindings & body]
  `((centring.ct/fn ~(take-nth 2 bindings)
      ~@body)
    ~@(take-nth 2 (rest bindings))))

(defn +-expander [a b]
  `(if (and (number? ~a) (number? ~b))
     (centring.ct/add ~a ~b)
     (+ ~a ~b)))

(defn with-gensyms-expander [syms & body]
  `(let ~(interleave syms (map #(list 'gensym (list 'quote %)) syms))
     ~@body))

(defn loop-expander [bindings & body]
  `(let ~(inject 'recur) ~bindings
     ~@body))