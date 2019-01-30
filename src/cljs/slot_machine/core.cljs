(ns slot-machine.core
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [slot-machine.events :as events]
   [slot-machine.views :as views]
   ))

(def debug? ^boolean goog.DEBUG)

(defn dev-setup []
  (when debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (mount-root))
