(ns philo.dot
  (:require [clojure.string :as s]))

(defn add-label [labels label node]
  (assoc labels label node))

(defn new-label [labels]
  (let [values (vals labels)]
    (if (empty? labels)
      1
      (inc (apply max values)))))

(defn gen-labels [names]
  (loop [labels {}
         rem names]
    (if (empty? rem)
      labels
      (recur (add-label labels (first rem) (new-label labels))
             (rest rem)))))

(defn convert-to-labels [data labels]
  (reduce (fn [tot [k vs]]
            (assoc tot k vs))
          {}
          (map (fn [[k vs]]
                 (list (labels k) (map labels vs)))
               data)))

(defn graph [& internals]
  (str "digraph {\n" (apply str internals) "\n}"))

(defn edge [from to]
  (str from " -> " to ";\n"))

(defn edges [from tos]
  (s/join "" (map (partial edge from) tos)))

(defn set-label [label node name-map]
  (str node " [ label = \"" (name-map label) "\" ];\n"))

(defn set-labels [labels name-map]
  (s/join "" (map #(apply set-label (concat % (list name-map))) labels)))

(defn graph-map [data nodes name-map]
  (let [labels (gen-labels nodes)
        converted-data (convert-to-labels data labels)]
    (graph
     (set-labels labels name-map)
     (s/join "" (map (fn [[key val]]
                       (edges key val))
                     converted-data)))))
