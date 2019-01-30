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
  (atom (map-indexed (fn [idx x]
                       (assoc x :roots_drop_order idx))
                     (repeatedly 10 random-drop))))

(def hub (random-drop))

(defn remove-nth [v n]
  (let [v (into [] v)]
    [(nth v n)
     (concat (subvec v 0 n)
             (subvec v (inc n)))]))

(defn insert-at [row-vec pos item]
  (println row-vec pos item)
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
                           best)))))))
  (dispatch tx tube (conj reply-v {:round @drops
                                   :hub hub

                                   :route @best-route})))

(defmethod handle-event :load-data
  [tube [_ _ reply-v]]
  (println "Hello " name)
  (dispatch tx tube (conj reply-v {:round @drops
                                   :hub hub})))


(defmethod handle-event :optimise
  [tube [_ _ reply-v]]
  (let [drops @drops
        drops (vec (map-indexed (fn [idx x] (assoc x :idx idx)) drops))
        matrix (generate-matrix (conj drops hub))
        hub-idx (count drops)
        {:keys [route] :as best} (optimise matrix (mapv :idx drops) hub-idx hub-idx 2000000)]
    (reset! best-route best)
    (dispatch tx tube (conj reply-v {:round drops
                                     :hub hub
                                     :route route}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defroutes routes
  (GET "/" [] (resource-response "index.html" {:root "public"}))
  (GET "/ws" [] (websocket-handler (receiver {:server #'handle-event})))
  (resources "/"))

(defn -main [& args]
  (def stop-server (run-server (bound-fn [req] (routes req)) {:port 9090})))
