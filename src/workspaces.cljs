(ns workspaces
  (:require
    [nubank.workspaces.card-types.react :refer [react-card]]
    [nubank.workspaces.core :refer [mount defcard]]
    [nubank.workspaces.model :as wsm]
    [reagent.core :as r]
    [spread-x :refer [spread-x-ui]]
    [spread-x-test]
    [eval-spread-test]))

(defonce init (mount))

(defcard spread-x
  {::wsm/align {:justify-content "left"}}
  (react-card
    (r/as-element [spread-x-ui])))