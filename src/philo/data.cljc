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
