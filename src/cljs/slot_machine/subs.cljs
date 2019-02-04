(ns slot-machine.subs
  (:require
   [re-frame.core :refer [reg-sub]]
   [clojure.string :as str]))

(reg-sub
 ::name
 (fn [db]
   (:name db)))

(reg-sub
 :get-in
 (fn [db [_ path]]
   (get-in db path)))

(reg-sub
 :current-route
 (fn [db [_]]
   (:route (last (get-in db [:data :routes])))))

(reg-sub
 :disrupted-customers
 (fn [db [_]]
   (let [customers (get-in db [:data :customers])
         {:keys [route times]} (last (get-in db [:data :routes]))]
     (->>  (map-indexed (fn [idx x]
                          (let [cust (nth customers x)
                                target-time (:target-time cust)
                                t (nth times idx)]
                            (if target-time
                              (assoc cust
                                     :estimated-time t
                                     :deviation (- target-time t)
                                     :abs-deviation (Math/abs (- target-time t)))
                              cust)))
                        route)
           (sort-by :abs-deviation >)
           (take 10)))))

(defn find-nth [pred coll]
  (loop [i 0]
    (if (= i (count coll))
      nil
      (if (pred (nth coll i))
        i
        (recur (inc i))))))

(reg-sub
 :drop-times-paths
 (fn [db [_]]
   (println "dtp3")
   (let [data (:data db)]
     (for [i-cust (range (dec (count (:customers data))))]
       (str "M"
            (.substring
                  (str/join " " (remove nil?
                                        (map-indexed
                                         (fn [idx r]
                                           (let [n (find-nth #(= i-cust %) (:route r))]
                                             (when n
                                               (str "L"
                                                    (int (* 0.2 (nth (:times r) n))) " " (* idx 20)))))
                                         (:routes data))))
                  1))))))
