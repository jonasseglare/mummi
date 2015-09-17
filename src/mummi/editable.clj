(ns mummi.editable
  (:require [mummi.common :refer [defprotocol2]]))

(defprotocol2 Editable
  "An object that can be edited in a gui"

  ;; result-fun will be called when
  ;; with the edited object if it was
  ;; edited.
  (edit [this result-fun]))
