(ns philo.data
  (:require [clojure.set :as s]))

(def struc set)

(def merge-edges (partial merge-with s/union))

(defn edge
  ([]
   {})
  ([from to]
   {from (struc (list to))})
  ([existing from to]
   (assoc existing
          from
          (conj (existing from (struc nil)) to))))

(defn all-elems [edge-map]
  (let [keys (set (keys edge-map))
        values (apply s/union (vals edge-map))]
    (s/union keys values)))

(defn- flatten-single [key val-set]
  (vec (map (fn [val] [key val]) val-set)))

(defn flat
  "Returns a collection of 2-tuples from -> to"
  [edge-map]
  (loop [acc []
         rem (vec edge-map)]
    (if (empty? rem)
      acc
      (recur (apply (partial conj acc)
                    (apply flatten-single (first rem)))
             (rest rem)))))
