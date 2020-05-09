(ns philo.core
  (:require [philo.wiki :as wiki]
            [philo.data :as data]
            [philo.dot :as dot]
            [clojure.tools.logging :as log]
            [clojure.core.async :as async]))

(def start "/wiki/Michel_Foucault")

(defn person [el]
  {:name (-> el :attrs :title)
   :path (-> el :attrs :href)})

(defn process [path]
  (log/info (str "processing " path))
  (let [doc (wiki/parse path)]
    {:name (-> (wiki/name doc) first :content first)
     :influences (map person (wiki/influences doc))
     :influenced (map person (wiki/influenced doc))}))

(defn influence-edges [path people-paths forward?]
  (loop [edges (data/edge)
         remaining people-paths]
    (if (empty? remaining)
      edges
      (recur (if forward?
               (data/edge edges path (first remaining))
               (data/edge edges (first remaining) path))
             (rest remaining)))))

(defn- inf-paths [info type]
  (map :path (info type)))

(defn edges [path info]
  (data/merge-edges
   (influence-edges path (inf-paths info :influences) false)
   (influence-edges path (inf-paths info :influenced) true)))

(defn process-chan [c]
  (async/go
    (loop [visited {}
           processed (list)
           latest (async/<! c)]
      (if latest
        (if (visited latest)
          (recur visited processed (async/<! c))
          (recur (assoc visited latest true)
                 (cons (process latest) processed)
                 (async/<! c)))
        processed))))


(defn go2
  ([path depth]
   (go2 path depth {}))
  ([path depth visited]
   (let [input (async/chan)
         processing (process-chan input)]
     (async/>!! input path)
     (async/>!! input "/wiki/Kazimierz_Twardowski")
     (async/close! input)
     (println (apply str (async/<!! processing))))))

(defn go
  ([path depth]
   (go path depth {}))
  ([path depth visited]
   (if (or (visited path) (= depth 0))
     (data/edge)
     (let [info (process path)
           page-data (edges path info)
           new-vis (assoc visited path true)
           next (concat (inf-paths info :influences)
                        (inf-paths info :influenced))]
       (data/merge-edges (reduce data/merge-edges
                                 (map #(go % (dec depth) new-vis)
                                      next))
                         page-data)))))

(defn -main [& args]
  (let [depth (Integer/parseInt (or (first args) "2"))
        philos (go start depth)
        nodes (data/all-elems philos)]
    (println (dot/graph-map philos nodes))))

