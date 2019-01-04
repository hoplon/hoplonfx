(ns hoplonfx.api
  (:require
    [hoplonfx.ui :refer [run]]
    [hoplonfx.util :refer :all])
  (:import
    [hoplonfx ApplicationShim]
    [javafx.application Application]))

(defonce stage
  (delay
    (with-promise [q]
      (Application/launch
        ApplicationShim
        (->> (meta (deftmp #(deliver q %)))
             ((juxt :ns :name))
             ((conxt (comp str ns-name) str))
             (into-array String))))))

(defn set-scene!
  [scene]
  @stage
  (run (.setScene @stage scene)))

(defn set-title!
  [title]
  @stage
  (run (.setTitle @stage title)))
