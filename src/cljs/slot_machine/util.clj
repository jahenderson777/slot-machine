(ns slot-machine.util
  (:require [clojure.set :as set]))

(defn remove-nth [v n]
  (let [v (into [] v)]
    [(nth v n)
     (concat (subvec v 0 n)
             (subvec v (inc n)))]))

(defn insert-at [row-vec pos item]
  (apply conj (subvec row-vec 0 pos) item (subvec row-vec pos)))

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
