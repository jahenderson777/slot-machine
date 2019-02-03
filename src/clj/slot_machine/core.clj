(ns slot-machine.core
    (:require [compojure.core :refer [GET defroutes]]
              [compojure.route :refer [resources]]
              [ring.util.response :refer [resource-response]]
              [org.httpkit.server :refer [run-server]]
              [pneumatic-tubes.core :refer [receiver transmitter dispatch]]
              [pneumatic-tubes.httpkit :refer [websocket-handler]]
              [slot-machine.util :refer :all])
    (:gen-class))

(def tx (transmitter))
(defmulti handle-event (fn [_ [_ event]] event))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn random-location []
  {:lat (rand-int 500)
   :lng (rand-int 500)})

(defn calc-matrix [locations]
  (vec (for [a locations]
         (vec (for [b locations]
                (distance-between (:lng a) (:lat a) (:lng b) (:lat b)))))))

(let [hub (random-location)
      customers (vec (for [i (range 4)] (assoc (random-location) :id i)))]
  (def db (atom {:customers customers
                 :matrix (calc-matrix (conj customers hub))
                 :routes [{:route [0 1 2 3]
                           :time 10
                           :cost 1000000
                           :disruption 0}]
                 :hub (random-location)})))

(defn cost-from-a-to-b [matrix a b]
  (nth (nth matrix a) b))

(defn cost-route [matrix route]
  (let [hub-idx (last-idx (first matrix))
      ;  _ (spy [route])
        ]
    (-> (reduce (fn [{:keys [cost last-idx times] :as m} next-idx]
                  (let [c (+ cost (cost-from-a-to-b matrix last-idx next-idx))]
                    (assoc! m :cost c
                            :times (conj times c)
                            :last-idx next-idx)
                    #_{:cost c
                                        ;:time c
                       :times (conj times c)
                       :last-idx next-idx}))
                (transient {:cost 0
                                        ;:time 0
                            :disruption 0
                            :times []
                            :last-idx hub-idx})
                (conj (vec route) hub-idx))
        (assoc! :route route)
        persistent!)))

(defn slot-in-idx [matrix route new-idx]
  (loop [i 0
         best (cost-route matrix (insert-at route 0 new-idx))]
    (if (= i (count route))
      best
      (let [new (cost-route matrix (insert-at route i new-idx))]
        (recur (inc i)
               (if (< (:cost new) (:cost best))
                 new
                 best))))))

(defn build-route [matrix order]
  (reduce (fn [{:keys [route]} next]
            (slot-in-idx matrix route next))
          {:route [(last-idx (first matrix))]}
          order))

(defn cust-target-times [customers routes min-deliveries num-last-routes]
  (for [id (range (count customers))]
    (let [times
          (reduce (fn [cust-times r]
                    (if-let [n (find-nth #(= % id) (:route r))]
                      (conj cust-times (nth (:times r) n))
                      cust-times))
                  []
                  (take num-last-routes (reverse routes)))]
      (if (>= (count times) min-deliveries)
        (median times)))))

(defn apply-target-times [db]
  (update db :customers (fn [customers]
                          (let [target-times (cust-target-times customers (:routes db) 1 7)]
                            (map-indexed (fn [idx c]
                                           (assoc c :target-time (nth target-times idx)))
                                  customers)))))

(defn disruption-costs [target-times route new-target-time]
  (let [num-drops (count route)]
    (->> (range (inc num-drops))
         (mapv (fn [n]
                 (loop [i 0 cost 0]
                   (if (= i num-drops)
                     cost
                     (let [target-time (nth target-times (nth route i))]
                       (recur (inc i)
                              (if target-time
                                (+ cost (if (< i n)
                                          (- new-target-time target-time)
                                          (- target-time new-target-time)))
                                cost))))))))))

(defn current-route [db]
  (vec (get-in db [:routes (last-idx (:routes db)) :route])))

(defn optimise [tube matrix route]
  (let [current-route (:route route)
        hub-idx (last-idx (first matrix))]
    (loop [i (* hub-idx 10)
           best route]
      (if (zero? i)
        best
        (let [new (build-route matrix (shuffle current-route))]
          (recur (dec i)
                 (if (< (:cost new) (:cost best))
                   (do (spy [i (:cost new) (:route new)])
                       (swap! db update :routes
                              (fn [routes]
                                (conj (pop routes)
                                      (update new :route pop))))
                       (when tube (dispatch tx tube [:assoc-in [:data] @db]))
                       new)
                   best))))))
  (println "done"))

(defmethod handle-event :toggle-customer
  [tube [e1 e2 id]]
  (let [active-on-route (some #(= id %) (current-route @db))]
    (if active-on-route
      (do (println "is active on route")
          (swap! db (fn [db]
                      (update-in db [:routes (last-idx (:routes db))]
                                 (fn [{:keys [route]}]
                                   (cost-route (:matrix db) (filter #(not= id %) route))))))
          (dispatch tx tube [:assoc-in [:data] @db]))
      (handle-event tube [e1 :add-customer
                          (get-in @db[:customers id :lng])
                          (get-in @db[:customers id :lat])]))))

(defmethod handle-event :add-customer
  [tube [_ _ lng lat]]
  (swap! db (fn [db]
              (let [new-id (count (:customers db))
                    new-cust {:lng lng :lat lat :id new-id}
                    db (update db :customers conj new-cust)
                    db (assoc db :matrix (calc-matrix (conj (:customers db) (:hub db))))

                    _ (println "sdlfkjsldkf2..")
                    _ (println (current-route db))
                    - (println "lskjlk")

                    route-with-new-cust-slotted-in
                    (spy (slot-in-idx (:matrix db)
                                      (current-route db)
                                      (last-idx (:customers db))))]
                
                (-> db
                    ;(assoc-in [:customers new-id] new-cust)
                    (assoc-in [:routes (last-idx (:routes db))]
                              route-with-new-cust-slotted-in)))))
  (dispatch tx tube [:assoc-in [:data] @db]))

(defmethod handle-event :new-route
  [tube _]
  (swap! db (fn [db]

              (update db :routes #(conj % (last %)))))
  (dispatch tx tube [:assoc-in [:data] @db]))

#_(defmethod handle-event :load-data
  [tube [_ _ reply-v]]
 ; (println "Hello " name)
  (dispatch tx tube (conj reply-v {:round @drops
                                   :hub hub})))




(defmethod handle-event :optimise
  [tube [_ _ reply-v]]
  (let [_db @db]
    (.start (Thread. #(optimise tube (:matrix _db) (last (:routes _db)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defroutes routes
  (GET "/" [] (resource-response "index.html" {:root "public"}))
  (GET "/ws" [] (websocket-handler (receiver {:server #'handle-event})))
  (resources "/"))

(defn -main [& args]
  (def stop-server (run-server (bound-fn [req] (routes req)) {:port 9090})))
