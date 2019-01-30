(ns slot-machine.views
  (:require
   [re-frame.core :refer [dispatch subscribe]]
   [slot-machine.events :as events]
   [slot-machine.subs :as subs]))

(defn <- [& v]
  (println (vec v))
  (deref (subscribe (vec v))))

(defn map-pin [x y]
  [:circle {:cx x :cy y :stroke "green" :r 10 :stroke-width 1 :fill "red"}])

(defn hub [x y]
  [:circle {:cx x :cy y :stroke "green" :r 10 :stroke-width 1 :fill "blue"}])

(defn svg-map []
  (let [drops (<- :get-in [:data :round])]
    [:svg {:width 600 :height 600
           :style {:border "1px solid grey"}
           :on-click (fn [evt] (let [e (.-target evt)
                                    dim (.getBoundingClientRect e)
                                    x (- (.-clientX evt) (.-left dim))
                                    y (- (.-clientY evt) (.-top dim))]
                                (dispatch [:server :add-drop [:assoc-in [:data]] x (- 550 y)])))}
     (doall (for [{:keys [latitude longitude] :as drop} drops]
              ^{:key drop}
              [map-pin longitude (- 550 latitude)]))
     (let [route (<- :get-in [:data :route])]
       (doall (for [q (range (dec (count route)))]
                (let [this (nth drops (nth route q))
                      next (nth drops (nth route (inc q)))]
                  ^{:key q}
                  [:line {:x1 (:longitude this) :y1 (- 550 (:latitude this))
                          :x2 (:longitude next) :y2 (- 550 (:latitude next))
                          :stroke-width 1
                          :stroke "green"}]))))
     (let [{:keys [latitude longitude]} (<- :get-in [:data :hub])]
       [hub longitude (- 550 latitude)])]))

(defn main-panel []
  [:div
   [:h1 "Slot machine"]
   [svg-map]
   [:div [:a {:on-click #(dispatch [:server :load-data [:assoc-in [:data]]])}
          "load data"]]
   [:div [:a {:on-click #(dispatch [:server :optimise [:assoc-in [:data]]])}
          "optimise"]]
   [:pre (with-out-str (cljs.pprint/pprint (<- :get-in [])))]
   ])
