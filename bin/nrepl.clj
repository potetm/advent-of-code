(ns com.potetm.nrepl
  (:require [nrepl.server :as srvr]))

(defonce s (srvr/start-server :port 0))

(spit (doto (java.io.File. ".nrepl-port")
        (.deleteOnExit))
      (:port s))

(println (str "REPL started on port " (:port s)))
