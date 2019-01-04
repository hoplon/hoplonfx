(set-env!
  :source-paths #{"src"}
  :resource-paths #{"resources"}
  :dependencies '[[org.clojure/clojure  "1.9.0"]
                  [backtick             "0.3.4"]
                  [camel-snake-kebab    "0.4.0"]
                  [compliment           "0.3.8" :scope "test"]
                  [virgil               "0.1.8" :scope "test"]])

(require '[virgil.boot :refer [javac*]])
