(ns philo.core
  (:require [philo.wiki :as wiki]
            [philo.data :as data]
            [philo.dot :as dot]
            [clojure.tools.logging :as log]
            [clojure.core.async :as async]))

(def start "/wiki/Michel_Foucault")

(def sec (partial * 1000))

(defn person [el]
  {:name (-> el :attrs :title)
   :path (-> el :attrs :href)})

(defn process [path]
  (log/info (str "processing " path))
  (let [doc (wiki/parse path)]
    {:name (-> (wiki/name doc) first :content first)
     :path path
     :influences (map person (wiki/influences doc))
     :influenced (map person (wiki/influenced doc))}))

(defn proc-to-paths
  "Returns a list of paths from `proc` the ouput of process"
  [proc]
  (concat (map :path (:influences proc))
          (map :path (:incluenced proc))))

(defn proc-to-names
  "Returns a map of path -> name"
  [proc]
  (loop [res {}
         remaining (concat (list proc)
                           (:influenced proc)
                           (:influences proc))]
    (if (empty? remaining)
      res
      (let [person (first remaining)]
        (recur (assoc res (:path person) (:name person))
               (rest remaining))))))

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

(defn edges [info]
  (data/merge-edges
   (influence-edges (:path info) (inf-paths info :influences) false)
   (influence-edges (:path info) (inf-paths info :influenced) true)))

(defn edge-chan
  "Takes the processed maps from `c` and constructs the relationship map."
  [c]
  (async/go
    (loop [res (data/edge)
           latest (async/<! c)]
      (if latest
        (recur (data/merge-edges res
                                 (edges latest))
               (async/<! c))
        res))))

(defn process-chan
  "Uses the process function to process everything in `c`.

  Outputs the result of processed paths to `out`. Does not
  revisit paths. Returns a map of path -> name."
  [c out]
  (async/go
    (loop [visited {}
           name-map {}
           latest (async/<! c)]
      (if latest
        (if (visited latest)
          (recur visited name-map (async/<! c))
          (let [data (process latest)
                paths (proc-to-paths data)]
            (async/go
              (async/>! out data)
              (doseq [path paths] (async/>! c path)))
            (recur (assoc visited latest true)
                   (merge name-map (proc-to-names data))
                   (async/<! c))))
        (do
          (async/close! out)
          name-map)))))

(defn go
  ([path timeout]
   (go path timeout {}))
  ([path timeout visited]
   (let [input (async/chan)
         edges (async/chan)
         time (async/timeout (sec timeout))
         processing (process-chan input edges)
         edging (edge-chan edges)]
     (async/>!! input path)
     (async/<!! time)
     (async/close! input)
     [(async/<!! edging)
      (async/<!! processing)])))

(defn -main [& args]
  (let [timeout (Integer/parseInt (or (first args) "20"))
        [philos name-map] (go start timeout)
        nodes (data/all-elems philos)]
    (log/info "Processed " (count nodes) " nodes in " timeout " seconds.")
    (println (dot/graph-map philos nodes name-map))))

