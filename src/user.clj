(ns user
  (:require [clojure.tools.namespace.repl :as r])
  (:import (clojure.lang PersistentQueue)
           (java.io Writer)))

(defmethod print-method PersistentQueue [q ^Writer w]
  (.write w "#q")
  (#'clojure.core/print-sequential "[" #'clojure.core/pr-on " " "]" q w))

(defn refresh []
  (r/refresh))
