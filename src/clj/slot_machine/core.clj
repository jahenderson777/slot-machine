(ns slot-machine.core
    (:require [compojure.core :refer [GET defroutes]]
              [compojure.route :refer [resources]]
              [ring.util.response :refer [resource-response]]
              [org.httpkit.server :refer [run-server]]
              [pneumatic-tubes.core :refer [receiver transmitter dispatch]]
              [pneumatic-tubes.httpkit :refer [websocket-handler]]
              [clojure.math.numeric-tower :as math]
              [slot-machine.util :refer :all])
    (:gen-class))

(def tx (transmitter))
(defmulti handle-event (fn [_ [_ event]] event))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn random-location []
  {:lat (rand-int 500)
   :lng (rand-int 500)})

(def db (atom {:customers (into {} (for [i (range 4)] [i (random-location)]))
               :routes [[0 1 2 3]]
               :selected-route 0
               :hub (random-location)}))

(def drops (atom []))
(def hub nil)

(defn distance-between [x1 y1 x2 y2]
  (math/sqrt (+ (math/expt (- x2 x1) 2)
                (math/expt (- y2 y1) 2))))

(defn generate-matrix [locations]
  (vec (for [a locations]
         (vec (for [b locations]
                (distance-between (:lng a) (:lat a) (:lng b) (:lat b)))))))

(defn cost-from-a-to-b [matrix a b]
  (nth (nth matrix a)
       b))

(defn cost-route [matrix route start-idx end-idx]
  (-> (reduce (fn [{:keys [cost last-idx]} next-idx]
                {:cost (+ cost (cost-from-a-to-b matrix last-idx next-idx))
                 :last-idx next-idx})
              {:cost 0
               :last-idx start-idx}
              (conj (vec route) end-idx))
      (assoc :route route)))

(def best-route (atom {:route nil :cost nil}))

(defn optimise [matrix route start-idx end-idx iterations]
  (loop [i 0
         best (cost-route matrix route start-idx end-idx)]
    (if (= i iterations)
      best
      (let [new (cost-route matrix (shuffle route) start-idx end-idx)]
        (recur (inc i)
               (if (< (:cost new) (:cost best))
                 new
                 best))))))

(defn slot-in-last [matrix drops route new-idx hub-idx]
  (loop [i 0
         best (cost-route matrix (insert-at route 0 new-idx) hub-idx hub-idx)]
    (if (= i (count route))
      best
      (let [new (cost-route matrix (insert-at route i new-idx) hub-idx hub-idx)]
        (recur (inc i)
               (if (< (:cost new) (:cost best))
                 new
                 best))))))

(defn build-route [matrix drops order hub-idx]
  (try
    (let [new-route
          (reduce (fn [route next]
                    (:route (slot-in-last matrix drops route next hub-idx)))
                  [hub-idx]
                  order)
          ret (cost-route matrix new-route hub-idx hub-idx)]
      ret)
    (catch Exception e (println e))))

(defn optimise [matrix route start-idx end-idx iterations]
  (let [drops (vec (map-indexed (fn [idx x] (assoc x :idx idx)) @drops))
        matrix (generate-matrix (conj drops hub))
        new-idx (dec (count drops))
        hub-idx (count drops)
        route (:route @best-route)
        _ (println route)]
    (reset! best-route
            (loop [i 0
                   best (cost-route matrix (insert-at route 0 new-idx) hub-idx hub-idx)]
              (if (= i (count drops))
                best
                (let [new (cost-route matrix (insert-at route i new-idx) hub-idx hub-idx)]
                  (recur (inc i)
                         (if (< (:cost new) (:cost best))
                           new
                           best))))))))

(defn disruption-costs [drops route new-idx]
  ;; (disruption-costs [{:avg-time 10} {:avg-time 20} {:avg-time 30} {:avg-time 40} {:avg-time 25}] [0 1 2 3] 4) 
  (let [;drops [{:avg-time 10} {:avg-time 20} {:avg-time 30} {:avg-time 40}]
                                        ;new-idx 1
                                        ;route [0 1 2 3]
        num-drops (count route)
        new-avg-time (:avg-time (nth drops new-idx))]
    (for [n (range (inc num-drops))]
      (loop [i 0 cost 0]
        (if (= i num-drops)
          cost
          (let [avg-time (:avg-time (nth drops (nth route i)))]
                                        ;(println i n)
            (recur (inc i)
                   (if avg-time
                     (+ cost (if (< i n)
                               (- new-avg-time avg-time)
                               (- avg-time new-avg-time)))
                     cost))))))))

(defn selected-route [db]
  (vec (get-in db [:routes (:selected-route db)])))

(defmethod handle-event :toggle-customer
  [tube [e1 e2 id]]
                                        ;(println "toggling" id)
  (let [active-on-route (some #(= id %) (spy (selected-route @db)))]
    (if active-on-route
      (do (println "is active on route")
         (swap! db (fn [db]
                      (update-in db [:routes (:selected-route db)]
                                 (fn [r] (filter #(not= id %) r)))))
          (dispatch tx tube [:assoc-in [:data] @db]))
      (handle-event tube [e1 :add-customer
                          (get-in @db[:customers id :lng])
                          (get-in @db[:customers id :lat])]))))

(defmethod handle-event :add-customer
  [tube [_ _ lng lat]]
  (swap! db (fn [db]
              (let [new-id (inc (apply max (keys (:customers db))))
                    new-cust {:lng lng :lat lat}
                    selected-route (selected-route db)
                    deliveries (vec (map (fn [id]
                                           (assoc (get-in db [:customers id]) :id id))
                                         selected-route))
                    ids (conj selected-route new-id)
                    matrix (generate-matrix (conj deliveries new-cust (:hub db)))
                    current-route (vec (range (count deliveries)))
                    new-idx (count deliveries)
                    hub-idx (inc new-idx)

                    result
                    (loop [i 0
                           best (cost-route matrix (insert-at current-route 0 new-idx) hub-idx hub-idx)]
                      (if (= i (count deliveries))
                        best
                        (let [new (cost-route matrix (insert-at current-route i new-idx) hub-idx hub-idx)]
                          (recur (inc i)
                                 (if (< (:cost new) (:cost best))
                                   new
                                   best)))))

                    route-with-new-cust-slotted-in
                    (mapv #(nth ids %) (:route result))]
                (-> db
                    (assoc-in [:customers new-id] new-cust)
                    (assoc-in [:routes (:selected-route db)] route-with-new-cust-slotted-in)))))
                                        ;(? deliveries)
  #_(reset! best-route
            )
  (dispatch tx tube [:assoc-in [:data] @db]))

(defmethod handle-event :add-drop
  [tube [_ _ reply-v longitude latitude]]
  (swap! drops conj {:longitude longitude :latitude latitude})
  
  (dispatch tx tube (conj reply-v {:round @drops
                                   :hub hub

                                   :route (:route @best-route)})))

(defmethod handle-event :load-data
  [tube [_ _ reply-v]]
 ; (println "Hello " name)
  (dispatch tx tube (conj reply-v {:round @drops
                                   :hub hub})))

(defmethod handle-event :optimise
  [tube [_ _ reply-v]]
  (try
    (let [_db @db
          selected-route (selected-route _db)
          deliveries (conj
                      (vec (map (fn [id]
                                        (assoc (get-in _db [:customers id]) :id id))
                                selected-route))
                      (:hub _db))
          ;ids (conj selected-route new-id)

          ;drops
          #_(conj (vec (map-indexed (fn [idx x] (assoc x :idx idx)) @drops))
                hub)
          matrix (generate-matrix deliveries)
          hub-idx (dec (count deliveries))
          indicies (range (dec (count deliveries)))]
      (loop [i (* hub-idx 50)
             best (build-route matrix deliveries (shuffle indicies) hub-idx)]
        (if (zero? i)
          best
          (let [new (build-route matrix deliveries (shuffle indicies) hub-idx)]
            (recur (dec i)
                   (if (< (:cost new) (:cost best))
                     (do (println i (:cost new) (:route new))
                         (swap! db assoc-in [:routes (:selected-route _db)]
                                (map #(nth selected-route %) (pop (:route new))))
                         (dispatch tx tube [:assoc-in [:data] @db])
                         new)
                     best))))))
    (println "done")
    (catch Exception e (println e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defroutes routes
  (GET "/" [] (resource-response "index.html" {:root "public"}))
  (GET "/ws" [] (websocket-handler (receiver {:server #'handle-event})))
  (resources "/"))

(defn -main [& args]
  (def stop-server (run-server (bound-fn [req] (routes req)) {:port 9090})))
