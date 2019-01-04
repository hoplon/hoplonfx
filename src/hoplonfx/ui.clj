(ns hoplonfx.ui
  (:require
    [clojure.data :as data]
    [hoplonfx.util :refer :all]
    [hoplonfx.cell :as c :refer [cell? formula]]
    [hoplonfx.codegen :refer [defjavafx-methods defjavafx-constructors]])
  (:import
    [hoplonfx ApplicationShim]
    [javafx.event EventHandler]
    [java.util Collections WeakHashMap]
    [javafx.application Platform Application]))

(defn run*
  [f]
  (Platform/runLater f))

(defmacro run
  [& body]
  `(run* (bound-fn [] ~@body)))

(def ^:dynamic *stage* nil)

(defonce primary-stage
  (delay
    (with-promise [q]
      (Application/launch
        ApplicationShim
        (->> (meta (deftmp #(deliver q %)))
             ((juxt :ns :name))
             ((conxt (comp str ns-name) str))
             (into-array String))))))

(defmacro with-stage
  [stage & body]
  `(binding [*stage* (deref ~stage)] ~@body))

(defmacro with-primary-stage
  [& body]
  `(with-stage primary-stage ~@body))

(defn set-scene!
  [scene]
  (run (.setScene *stage* scene)))

(defn set-title!
  [title]
  (run (.setTitle *stage* title)))

(def ^:private metastore
  (Collections/synchronizedMap (WeakHashMap.)))

(defn store
  ([k] (.get metastore k))
  ([k v] (.put metastore k v)))

(defprotocol INode
  (-data [this])
  (-bind [this k v])
  (-unbind [this token]))

(defn node?
  [node]
  (when (satisfies? INode node) node))

(defn node
  [root]
  (or (and (satisfies? INode root) root)
      (store root)))

(defn data
  [obj]
  (when (satisfies? INode obj) (-data obj)))

(defn bind
  [obj meth & cells]
  (when (satisfies? INode obj) (-bind obj meth cells)))

(defn unbind
  [obj token]
  (when (satisfies? INode obj) (-unbind obj token)))

(defn h*
  [f]
  (reify EventHandler
    (handle [this e] (f this e))))

(defmacro h
  [[& args] & body]
  `(h* (fn [~@args] ~@body)))

(defn- split-args
  [args]
  ((conxt identity flatten) (split-with fn? args)))

(defn- apply-args
  [node args]
  (let [[attrs kids] (split-args args)]
    (with-let [_ node]
      (doseq [f attrs] (f node))
      (swap! (.-kids node) into kids))))

(defn- update-kids
  [node]
  (run (some->> @(.-kids node)
                (map #(if (cell? %) @% %))
                (seq)
                (flatten)
                (filter (partial instance? javafx.scene.Node))
                (fxnodes)
                (.setAll (.getChildren (.-root node))))))

(deftype+ Node
  [root binds kids data]
  :ifn-delegate
  (fn [this & args]
    (-> this (apply-args args) (.-root)))
  INode
  (-data [this] data)
  (-bind [this f cells]
    (let [c (formula vector cells)]
      (doto [c (with-watch c [xs] (run (apply f root xs)))]
        (->> (swap! binds conj)))))
  (-unbind [this token]
    (swap! binds disj token)
    (remove-watch (first token) (second token))))

(defjavafx-constructors
  [root-class & args]
  (let [root (apply-constructor root-class args)]
    (with-let [node (Node. root (c/cell #{}) (c/cell []) (c/cell {}))]
      (store root node)
      (with-watch (.-kids node) [old new]
        (let [old-cells (set (filter cell? old))
              new-cells (set (filter cell? new))
              [rem add] (map set (data/diff old-cells new-cells))]
          (doseq [c rem] (remove-watch c node))
          (doseq [c add] (add-watch c node (fn [& _] (update-kids node))))
          (update-kids node))))))

(defjavafx-methods
  [method & args]
  (fn [node]
    (->> args (apply bind node (fn [root & xs]
                                 (apply-method root method xs))))))

(defn scene
  [& args]
  (fn [& xs]
    (let [[attrs [kid & _]] (split-args xs)]
      (apply-constructor javafx.scene.Scene (cons kid args)))))
