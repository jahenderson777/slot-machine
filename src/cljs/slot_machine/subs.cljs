(ns slot-machine.subs
  (:require
   [re-frame.core :refer [reg-sub]]))

(reg-sub
 ::name
 (fn [db]
   (:name db)))

(reg-sub
 :get-in
 (fn [db [_ path]]
   (get-in db path)))
