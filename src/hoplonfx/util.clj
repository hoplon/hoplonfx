(ns hoplonfx.util
  (:require
    [clojure.pprint :refer [pprint]]
    [backtick :refer [template]])
  (:import
    [clojure.lang Reflector]))

(javafx.embed.swing.JFXPanel.)

(defn requisolve
  [varsym]
  (require (symbol (namespace varsym)))
  @(resolve varsym))

(defn gensyms
  ([n]
   (gensyms "arg%d__" n))
  ([prefix n]
   (map (comp gensym (partial format prefix)) (range n))))

(defn genkey
  []
  (keyword (name (gensym))))

(deftype WeakRef
  [ref]
  clojure.lang.IDeref
  (deref [this] (.get ref)))

(defn weakref
  [referent]
  (WeakRef. (java.lang.ref.WeakReference. referent)))

(defmacro with-let
  [[bind & _ :as bindings] & body]
  `(let [~@bindings] ~@body ~bind))

(defmacro doto->>
  [form & body]
  (let [sym  (gensym)]
    `(with-let [~sym ~form]
       ~@(map #(concat % (list sym))
              (map #(if (seq? %) % (list %)) body)))))

(defmacro with-promise
  [[bind] & body]
  `(let [~bind (promise)]
     (.start (Thread. (fn [] ~@body)))
     (deref ~bind)))

(defn do-watch
  ([c f] (do-watch c nil f))
  ([c init f] (do-watch c (gensym) init f))
  ([c k init f]
   (with-let [k k]
     (f init @c)
     (add-watch c k (fn [_ _ old new] (f old new))))))

(defmacro with-watch
  [c & args]
  (let [k?        (not (vector? (first args)))
        k         (if k? (first args) (genkey))
        bindings  ((if k? second first) args)
        body      (drop (if k? 2 1) args)
        [old new] (reverse (take 2 (into (gensyms 2) bindings)))]
    `(do-watch ~c ~k nil (fn [~old ~new] ~@body))))

(defn watch-once
  [c f]
  (with-let [k (gensym)]
    (add-watch c k (fn [_ _ old new]
                     (remove-watch c k)
                     (f old new)))))

(defmacro with-once
  [c [b1 b2 :as bindings] & body]
  (let [[old new] (case (count bindings)
                    0 [(gensym) (gensym)]
                    1 [(gensym) b1]
                    [b1 b2])]
    `(watch-once ~c (fn [~old ~new] ~@body))))

(defn split-by
  [f coll]
  [(filter f coll) (filter (complement f) coll)])

(defmacro guard
  [& body]
  `(try ~@body (catch Throwable _#)))

(defn guarded
  [f]
  #(guard (apply f %&)))

(defn pred
  [f]
  #(when (f %) %))

(defn conxt
  [& fns]
  (fn [vals] (mapv #(%1 %2) fns vals)))

(defn fxnodes
  [xs]
  (into-array javafx.scene.Node xs))

(defmacro deftmp
  [value]
  `(def ~(symbol (str "tmp-" (java.util.UUID/randomUUID))) ~value))

(defn macrospy
  [form]
  (throw (ex-info (with-out-str (pprint form)) {:form form})))

(defn apply-method
  [obj method args]
  (Reflector/invokeInstanceMethod
    obj (name method) (into-array Object args)))

(defn apply-constructor
  [class args]
  (Reflector/invokeConstructor class (into-array Object args)))

(defn- defthing-plus-helper
  [operation typename [& fields] & body]
  (let [maxargs 19
        optset  #{:ifn-delegate}
        pairs   (->> (partition-all 2 body))
        myopts  (->> (take-while (comp keyword? first) pairs)
                     (filter (comp optset first))
                     (reduce (partial apply assoc) {}))
        body    (mapcat identity (remove (comp optset first) pairs))
        syms    (comp gensyms inc)]
    (template
      (~operation ~typename [~@fields]
        ~@body
        ~@(when-let [delegate (:ifn-delegate myopts)]
            (template
              [clojure.lang.IFn
               ~@(for [i (range (+ 2 maxargs)) :let [args (syms i)]]
                   (template (invoke [~@args] (~delegate ~@args))))
               ~(let [args (concat (syms maxargs) (list '& (gensym "more__")))
                      params (remove (partial = '&) args)]
                  (template (invoke [~@args] (apply ~delegate ~@params))))
               (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))]))))))

(defmacro deftype+
  [typename [& fields] & body]
  `~(apply defthing-plus-helper `deftype typename fields body))

(defmacro defrecord+
  [typename [& fields] & body]
  `~(apply defthing-plus-helper `defrecord typename fields body))
