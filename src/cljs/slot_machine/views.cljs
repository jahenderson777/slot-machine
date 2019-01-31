(ns slot-machine.views
  (:require
   [re-frame.core :refer [dispatch subscribe]]
   [reagent.core :as r]
   [cljsjs.leaflet]
   [slot-machine.events :as events]
   [slot-machine.subs :as subs]))

(defn <- [& v]
  (println (vec v))
  (deref (subscribe (vec v))))

(defn map-pin [x y]
  [:circle {:cx x :cy y :stroke "green" :r 6 :stroke-width 1 :fill "red"}])

(defn hub [x y]
  [:circle {:cx x :cy y :stroke "green" :r 10 :stroke-width 1 :fill "blue"}])

(defn svg-map []
  (let [w 1000
        h 600
        h2 (- h 50)
        drops (<- :get-in [:data :round])]
    [:svg {:width w :height h
           :style {:border "1px solid grey"}
           :on-click (fn [evt] (let [e (.-target evt)
                                    dim (.getBoundingClientRect e)
                                    x (- (.-clientX evt) (.-left dim))
                                    y (- (.-clientY evt) (.-top dim))]
                                (dispatch [:server :add-drop [:assoc-in [:data]] x (- h2 y)])))}
     (doall (for [{:keys [latitude longitude] :as drop} drops]
              ^{:key drop}
              [map-pin longitude (- h2 latitude)]))
     (let [route (<- :get-in [:data :route])]
       (doall (for [q (range (dec (count route)))]
                (let [this (nth drops (nth route q))
                      next (nth drops (nth route (inc q)))]
                  ^{:key q}
                  [:line {:x1 (:longitude this) :y1 (- h2 (:latitude this))
                          :x2 (:longitude next) :y2 (- h2 (:latitude next))
                          :stroke-width 3
                          :stroke "green"}]))))
     (let [{:keys [latitude longitude]} (<- :get-in [:data :hub])]
       [hub longitude (- h2 latitude)])]))

(def attribution "Map data: <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a>")

(def url "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")

(defn draw-route [map route]
  (println "here")
  (-> js/L
      (.geoJSON (clj->js
                 route
                 #_[{"type" "LineString"
                           "coordinates" [[52.50284 0 ]
                                          [ 50.50284 55]
                                          ]}
                          {"type" "LineString"
                           "coordinates" [[ 0 52.50284]
                                          [ 60.50284 14.44096]
                                          ]}])
                (clj->js {:style {:color "red" :weight 5 :opacity 1}}))
      (.addTo map)))

(defn add-optimized-routes [map couriers opt-routes]
  (doseq [route opt-routes]
    (draw-route map (:points route))))

(defn draw-tile-layer [map]
  (-> js/L
      (.tileLayer url (clj->js {:attribution attribution}))
      (.addTo map)))

(defn draw-marker [map coords]
  (-> js/L
      (.marker (clj->js coords))
      (.addTo map)
      (.bindPopup "<b>Hello world!</b><br />This is us.")
      (.openPopup)))

(defn map-did-update
  "updating map after component is rerendered"
  [map-component mapspec]
  (let [center (:center mapspec)
        zoom       (:zoom mapspec)
        opt-routes (:opt-routes mapspec)]
    (draw-tile-layer map-component)
    (draw-marker map-component center)
    (print "mapspec: " mapspec)
    (print "opt-routes: " opt-routes)
    (when opt-routes
      (add-optimized-routes map-component [:vehicle1 :vehicle2 :vehicle3 :vehicle4] opt-routes))))

(defn map-render
  "rendering map component"
  [mapspec]
  (let [width (:width mapspec)
        height (:height mapspec)]
    [:div#map {:style {:height height :width width}}]))

(defn map-component
  "form-3 component to draw map"
  [mapspec]
  (let [map-component-atom (atom nil)]
    (r/create-class {:get-initial-state    (fn [_] {:mapspec mapspec})
                     :display-name         (:id mapspec)
                     :component-did-mount  (fn [comp]
                                             (let [map-component (.setView
                                                                  (.map js/L "map")
                                                                  (clj->js (:center mapspec))
                                                                  (clj->js (:zoom mapspec)))]
                                               (reset! map-component-atom map-component)
                                               (map-did-update map-component mapspec)))
                     :component-did-update (fn [comp]
                                               (map-did-update @map-component-atom mapspec))
                     :reagent-render       map-render})))

#_(defn update-map-button []
  [:button {:type "button" :on-click #(rf/dispatch [:request-event])} "Event"])


(defn main-panel []
  [:div
   [:h1 "Slot machine V2"]
   [map-component {:id         "dispatcher-map"
                   :width      "500px" :height "400px"
                   :center     [52.50284 13.44096]
                   :zoom       12
                   :opt-routes [{:points {
                  "type" "FeatureCollection",
                  "features" [{
                                "type" "Feature",
                                "geometry" {
                                             "type" "LineString",
                                             "coordinates" [[-45, 0],[45, 0]]
                                             },
                                "properties" {
                                               "color" "red"
                                               }
                                }, {
                                    "type" "Feature",
                                    "geometry" {
                                                 "type" "LineString",
                                                 "coordinates" [[0, -45],[0, 45]]
                                                 },
                                    "properties" {
                                                   "color" "yellow"
                                                   }
                                    }]
                  }}]}]
   [svg-map]
   [:div [:a {:on-click #(dispatch [:server :load-data [:assoc-in [:data]]])}
          "load data"]]
   [:div [:a {:on-click #(dispatch [:server :optimise [:assoc-in [:data]]])}
          "optimise"]]
   ;[:pre (with-out-str (cljs.pprint/pprint (<- :get-in [])))]
   ])
