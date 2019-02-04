(ns slot-machine.views
  (:require
   [re-frame.core :refer [dispatch subscribe]]
   [reagent.core :as r]
   [cljsjs.leaflet]
   [slot-machine.events :as events]
   [slot-machine.subs :as subs]))

#_(defmacro ? [x]
  (let [line (:line (meta &form))
        file *file*]
    `(let [x# ~x]
       (println (pr-str '~x) "is" (pr-str x#)
                (str "; (" ~file ":" ~line ")"))
       x#)))

(defn <- [& v]
  (println (vec v))
  (deref (subscribe (vec v))))

(defn map-pin [x y id]
  [:circle.pin {:on-click (fn [evt]
                            (dispatch [:server :toggle-customer id])
                            (.stopPropagation evt))
                :cx x :cy y :stroke "green" :r 8 :stroke-width 1 :fill "red"}])

(defn hub [x y]
  [:circle {:cx x :cy y :stroke "green" :r 10 :stroke-width 1 :fill "blue"}])

(defn svg-map []
  (let [w 1000
        h 600
        h2 (- h 50)
        drops (<- :get-in [:data :customers])]
    [:svg {:width w :height h
           :style {:border "1px solid grey"}
           :on-click (fn [evt] (let [e (.-target evt)
                                    dim (.getBoundingClientRect e)
                                    x (- (.-clientX evt) (.-left dim))
                                    y (- (.-clientY evt) (.-top dim))]
                                (dispatch [:server :add-customer x (- h2 y)])))}
     (let [route (<- :current-route)]
       (println "route=" route)
       (doall (for [q (range (dec (count route)))]
                (let [this (get drops (nth route q))
                      next (get drops (nth route (inc q)))]
                  (println this next)
                  ^{:key q}
                  [:line {:x1 (:lng this) :y1 (- h2 (:lat this))
                          :x2 (:lng next) :y2 (- h2 (:lat next))
                          :stroke-width 3
                          :stroke "green"}]))))
     (doall (for [{:keys [id lat lng] :as drop} drops]
              ^{:key id}
              [map-pin lng (- h2 lat) id]))
     (let [{:keys [lat lng]} (<- :get-in [:data :hub])]
       [hub lng (- h2 lat)])]))

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

(defn hsl [h s l]
  (str "hsl(" h "," s "%," l "%)"))

(defn rand-hsl []
  (hsl (rand-int 355) (rand-int 100) 50))

(defn main-panel []
  [:div
   [:h1 "Slot machine V3"]
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
   [:div
    [:h3 "Route costs"]
    [:div "Time = " (:cost (last (<- :get-in [:data :routes])))]
    [:div "Disruption (mean deviation from target time) = " (:disruption (last (<- :get-in [:data :routes])))]]
   [:div.slidecontainer
    [:h3 "Anti-disruption slider"]
    [:span (<- :get-in [:data :anti-disrupt])]
    [:input.slider {:type "range"
                    :min 0
                    :max 400
                                        ;:value 
                    :on-change (fn [e]
                                 (dispatch [:server :set-anti-disrupt (* 0.01 (js/parseInt (.-target.value e)))]))
                    }]]
   [:div 
    [:button {:class "btn btn-primary"
              :type "button"
              :on-click #(dispatch [:server :optimise [:assoc-in [:data]]])}
     "optimise"]
    [:button {:class "btn btn-primary"
              :type "button"
              :on-click #(dispatch [:server :new-route [:assoc-in [:data]]])}
     "next week"]]
   [:svg {:width 900 :height 700
          :style {:border "1px solid grey"}}
    (doall (map-indexed
            (fn [idx p]
              ^{:key idx}
              [:path {:stroke (rand-hsl)
                      :fill "none"
                      :d p}])
            (<- :drop-times-paths)
            ))]
   [:pre (str
          (<- :drop-times-paths)
          #_(<- :disrupted-customers))]
   ])
