(ns hoplonfx.demo.app
  (:require
    [hoplonfx.ui :as ui]
    [hoplonfx.api :refer :all]
    [hoplonfx.cell :refer :all])
  (:refer hoplonfx.ui :rename {cell fxcell})
  (:import [javafx.geometry Insets Pos]))

(defonce n (cell 0))

(defn -main [& _]
  (set-title! "hello world")
  (set-scene!
    ((scene 300 250)
      ((stack-pane)
        ((v-box)
          (-set-alignment Pos/CENTER)
          ((label)
            (-set-padding (Insets. 12))
            (-set-text (cell= (format "clicks: %d" n))))
          ((button)
            (-set-text "doit")
            (-set-on-action (h [_ _] (swap! n inc)))))))))
