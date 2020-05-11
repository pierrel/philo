(ns philo.csv
  (:require [philo.data :as data]))

(def header "influencer,influenced")

(defn rel [from to]
  (str from "," to))

(defn print-csv [data name-map]
  (println header)
  (doseq [relation (data/flat data)]
    (println (apply rel (map name-map relation)))))
