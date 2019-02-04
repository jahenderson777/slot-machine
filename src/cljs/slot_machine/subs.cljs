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
