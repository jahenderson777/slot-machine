(ns slot-machine.core
    (:require [compojure.core :refer [GET defroutes]]
              [compojure.route :refer [resources]]
              [ring.util.response :refer [resource-response]]
              [org.httpkit.server :refer [run-server]]
              [pneumatic-tubes.core :refer [receiver transmitter dispatch]]
              [pneumatic-tubes.httpkit :refer [websocket-handler]]
              [clojure.math.numeric-tower :as math])
  (:gen-class))

(def tx (transmitter))
(defmulti handle-event (fn [_ [_ event]] event))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn random-drop []
  {:latitude (rand-int 500)
   :longitude (rand-int 500)})

(def drops
  (atom (vec (map-indexed (fn [idx x]
                            (assoc x :roots_drop_order idx))
                          (repeatedly 4 random-drop)))))

(def hub (random-drop))

(defn remove-nth [v n]
  (let [v (into [] v)]
    [(nth v n)
     (concat (subvec v 0 n)
             (subvec v (inc n)))]))

(defn insert-at [row-vec pos item]
  ;(println row-vec pos item)
  (apply conj (subvec row-vec 0 pos) item (subvec row-vec pos)))

(defn distance-between [x1 y1 x2 y2]
  (math/sqrt (+ (math/expt (- x2 x1) 2)
                (math/expt (- y2 y1) 2))))

(defn generate-matrix [drops]
  (vec (for [a drops]
         (vec (for [b drops]
                (distance-between (:longitude a) (:latitude a) (:longitude b) (:latitude  b)))))))

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
  ;(println "slot-in-last" route new-idx hub-idx)
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
  ;(throw (Exception. "sldjfh"))
;  (println "build-route3vxxxx22222" order hub-idx )
  (try
    (let [new-route                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
          (reduce (fn [route next]
                    (:route (slot-in-last matrix drops route next hub-idx)))
                  [hub-idx]
                  order)
          ret (cost-route matrix new-route hub-idx hub-idx)]
   ;   (println "fooo " ret)
      
      ret
      )
    (catch Exception e (println e))))



#_(slot-in-last [{:latitude 10 :longitude 10}
               {:latitude 14 :longitude 12}
               {:latitude 12 :longitude 15}
               {:latitude 13 :longitude 11}]
              [1 2 0])


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

(comment
  [6 1 7 2 0 5 3 4 8]
  (let [
        drops (vec (map-indexed (fn [idx x] (assoc x :idx idx)) drops))
        matrix (generate-matrix (conj drops hub))
        [new-drop some-drops] (remove-nth drops (rand-int (count drops)))
        hub-idx (count drops)
        {:keys [route]} (optimise matrix (mapv :idx some-drops) hub-idx hub-idx 200000)
        new-idx (:idx new-drop)

        slotted-in
        (loop [i 0
               best (cost-route matrix (insert-at route 0 new-idx) hub-idx hub-idx)]
          (if (= i (count some-drops))
            best
            (let [new (cost-route matrix (insert-at route i new-idx) hub-idx hub-idx)]
              (recur (inc i)
                     (if (< (:cost new) (:cost best))
                       new
                       best)))))]
    {:new-drop new-drop
     :hub-idx hub-idx
     :starting-route route
     :slotted-in slotted-in})

  (generate-matrix drops)

  (distance-between 0 0 2 3)

  (math/expt 4 2)
  (remove-nth [1 2 3 4 5] 3)

  )

(defmethod handle-event :add-drop
  [tube [_ _ reply-v longitude latitude]]
  (swap! drops conj {:longitude longitude :latitude latitude})
  (let [drops (vec (map-indexed (fn [idx x] (assoc x :idx idx)) @drops))
        matrix (generate-matrix (conj drops hub))
        new-idx (dec (count drops))
        hub-idx (count drops)
        route (:route @best-route)
       ; _ (println route)
        ]
    (reset! best-route
            (loop [i 0
                   best (cost-route matrix (insert-at route 0 new-idx) hub-idx hub-idx)]
              (if (= i (count drops))
                best
                (let [new (cost-route matrix (insert-at route i new-idx) hub-idx hub-idx)]
                  (recur (inc i)
                         (if (< (:cost new) (:cost best))
                           new
                           best)))))))
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
    (let [drops (conj (vec (map-indexed (fn [idx x] (assoc x :idx idx)) @drops))
                      hub)
          matrix (generate-matrix drops)
          hub-idx (dec (count drops))
          indicies (range (dec (count drops)))]
      (loop [i (* hub-idx 50)
             best (build-route matrix drops (shuffle indicies) hub-idx)]
        (if (zero? i)
          best
          (let [new (build-route matrix drops (shuffle indicies) hub-idx)]
            (recur (dec i)
                   (if (< (:cost new) (:cost best))
                     (do (println i (:cost new) (:route new))
                         (reset! best-route new)
                         (dispatch tx tube (conj reply-v {:round drops
                                                          :hub hub
                                                          :route (:route new)}))
                         new)
                     best))))))
    (println "done")
    (catch Exception e (println e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defroutes routes
  (GET "/" [] (resource-response "index.html" {:root "public"}))
  (GET "/ws" [] (websocket-handler (receiver {:server #'handle-event})))
  (resources "/"))

(defn -main [& args]q
  (def stop-server (run-server (bound-fn [req] (routes req)) {:port 9090})))
