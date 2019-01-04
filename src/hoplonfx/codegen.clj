(ns hoplonfx.codegen
  (:import
    [java.lang.reflect Modifier])
  (:require
    ;; Using require/refer below to avoid the runtime dependency.
    #_[compliment.utils :as compliment]
    [clojure.set :refer [intersection]]
    [camel-snake-kebab.core :refer [->kebab-case-string]]
    [hoplonfx.util :refer [apply-constructor gensyms conxt with-let]]))

;;;; runtime codegen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def classlist-path
  "resources/classlist.edn")

(def classlist
  (delay (read-string (slurp classlist-path))))

(defmacro define-javafx-constructors
  [[bind-class & args] & body]
  `(do ~@(for [{:keys [java clj]} (:classes @classlist)]
           `(defn ~(symbol clj) [~@args]
              (let [~bind-class ~(Class/forName java)] ~@body)))))

(defmacro define-javafx-methods
  [[bind-method & args] & body]
  `(do ~@(for [{:keys [java clj]} (:methods @classlist)]
           `(defn ~(symbol clj) [~@args]
              (let [~bind-method ~java] ~@body)))))

;;;; classlist creation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn top-package
  [class]
  (-> class .getPackage .getName (.replaceAll "\\..*" "")))

(defn all-classes
  []
  (require 'compliment.utils)
  (@(resolve 'compliment.utils/classes-on-classpath)))

(defn superclasses-of
  [subclass]
  (set (take-while identity (iterate (memfn getSuperclass) subclass))))

(defn subclasses-of
  [superclass & {:keys [allowed-modifiers]}]
  (->> ((all-classes) (top-package superclass))
       (map (comp (juxt identity
                        superclasses-of
                        (memfn getModifiers))
                  #(Class/forName %)))
       (keep (fn [[a b c]]
               (when (and (contains? b superclass)
                          (or (not allowed-modifiers) (allowed-modifiers c)))
                 a)))))

(defn javafx-classes
  []
  (subclasses-of javafx.scene.Node :allowed-modifiers #{1}))

(defn enriched-classes
  [classes]
  (->> classes (map (fn [x]
                      {:java (.getName x)
                       :clj  (->kebab-case-string (.getSimpleName x))}))))

(defn enriched-methods
  [classes]
  (let [m       Modifier/PUBLIC
        public? #(when (< 0 (bit-and m (.getModifiers %))) (.getName %))]
    (->> (mapcat (memfn getMethods) classes)
         (keep public?)
         (into (sorted-set))
         (map (fn [x] {:java x :clj (str "-" (->kebab-case-string x))})))))

(defn javafx-classes-edn
  []
  (let [classes (javafx-classes)]
    (str (format ";; Generated %s.\n" (java.util.Date.))
         (pr-str {:classes (vec (enriched-classes classes))
                  :methods (vec (enriched-methods classes))}))))

;;;; classlist update ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn write-classlist-file!
  [& {:keys [dryrun]}]
  (->> (javafx-classes-edn)
       ((if dryrun println (partial spit classlist-path)))))

(comment

  (identity *e)

  ;; testing it
  (write-classlist-file! :dryrun true)
  (read-string (with-out-str (write-classlist-file! :dryrun true)))

  ;; doing it
  (write-classlist-file!)
  (read-string (slurp classlist-path))

  )
