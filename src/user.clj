(ns user
  (:require [shadow.cljs.devtools.api :as shadow]
            [shadow.cljs.devtools.server :as server]
            [main :refer [-main]]))

(defn start! []
  (server/start!)
  (shadow/watch :spread-x)
  (shadow/watch :workspaces)
  (-main))
