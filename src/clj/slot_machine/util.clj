(ns slot-machine.util
  (:require [clojure.set :as set]
            [clojure.math.numeric-tower :as math]))

(defn distance-between [x1 y1 x2 y2]
  (math/sqrt (+ (math/expt (- x2 x1) 2)
                (math/expt (- y2 y1) 2))))

(defn remove-nth [v n]
  (let [v (into [] v)]
    [(nth v n)
     (concat (subvec v 0 n)
             (subvec v (inc n)))]))

(defn insert-at [row-vec pos item]
  (apply conj (subvec row-vec 0 pos) item (subvec row-vec pos)))

(defn last-idx [coll]
  (dec (count coll)))

(defmacro spy [x]
  (let [line (:line (meta &form))
        file *file*]
    `(let [x# ~x]
       (println (pr-str '~x) "is" (pr-str x#)
                (str "; (" ~file ":" ~line ")"))
       x#)))

#_(defn- seqzip
  "returns a sequence of [[ value-left] [value-right]....]  padding with nulls for shorter sequences "
  [left right]
  (loop [list [] a left b right]
    (if (or (seq a) (seq b))
      (recur (conj list [(first a) (first b)] ) (rest a) (rest b))
       list)))

#_(defn- recursive-diff-merge
  " Merge two structures recusively , taking non-nil values from sequences and maps and merging sets" 
  [part-state original-state]
  (cond
    (sequential? part-state) (map (fn [[l r]] (recursive-diff-merge l r)) (seqzip part-state original-state))
    (map? part-state) (merge-with recursive-diff-merge part-state original-state)
    (set? part-state) (set/union part-state original-state)
    (nil? part-state ) original-state
    :default part-state))

#_(defn undiff
  "returns the state of x after reversing the changes described by a diff against
   an earlier state (where before and after are the first two elements of the diff)"
  [x before after]
  (let [[a _ _] (clojure.data/diff x after)]
    (recursive-diff-merge a before)))


(defn find-nth [pred coll]
  (loop [i 0]
    (if (= i (count coll))
      nil
      (if (pred (nth coll i))
        i
        (recur (inc i))))))

(defn quantile
  ([p vs]
   (let [svs (sort vs)]
     (quantile p (count vs) svs (first svs) (last svs))))
  ([p c svs mn mx]
   (let [pic (* p (inc c))
         k (int pic)
         d (- pic k)
         ndk (if (zero? k) mn (nth svs (dec k)))]
     (cond
       (zero? k) mn
       (= c (dec k)) mx
       (= c k) mx
       :else (+ ndk (* d (- (nth svs k) ndk)))))))

(defn median
  ([vs] (quantile 0.5 vs))
  ([sz svs mn mx] (quantile 0.5 sz svs mn mx)))

(defn mean
  ([vs] (mean (reduce + vs) (count vs)))
  ([sm sz] (/ sm sz)))

(defn standard-deviation
  ([vs]
   (standard-deviation vs (count vs) (mean vs)))
  ([vs sz u]
   (Math/sqrt (/ (reduce + (map #(Math/pow (- % u) 2) vs))
                 sz))))
