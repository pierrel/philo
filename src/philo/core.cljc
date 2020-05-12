(ns philo.core
  (:require [philo.wiki :as wiki]
            [philo.data :as data]
            [philo.dot :as dot]
            [philo.csv :as csv]
            [clojure.tools.logging :as log]
            [clojure.core.async :as async]))

(def start "/wiki/Michel_Foucault")

(def sec (partial * 1000))

(defn person-from-rel
  "An element on the page will not have relationship information"
  [el]
  {:name (wiki/rel-name el)
   :path (wiki/rel-path el)})

(defn rel-from-doc [doc]
  {:influences (map person-from-rel (wiki/influences doc))
   :influenced (map person-from-rel (wiki/influenced doc))})

(defn person-from-doc [doc path]
  {:name (wiki/heading doc)
   :path path})

(defn personrel-from-doc [doc path]
  (merge (person-from-doc doc path)
         (rel-from-doc doc)))

(defn process [path]
  (log/info (str "processing " path))
  (personrel-from-doc (wiki/parse path)
                   path))

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
  (async/go-loop [res (data/edge)
                  latest (async/<! c)]
    (if latest
      (recur (data/merge-edges res
                               (edges latest))
             (async/<! c))
      res)))

(defn process-chan
  "Uses the process function to process everything in `c`.

  Outputs the result of processed paths to `out`. Does not
  revisit paths. Returns a map of path -> name."
  [in out]
  (async/go-loop [visited {}
         name-map {}
         latest (async/<! in)]
    (if latest
      (if (visited latest)
        (recur visited name-map (async/<! in))
        (let [data (process latest)
              paths (proc-to-paths data)]
          (async/go
            (async/>! out data)
            (doseq [path paths] (async/>! in path)))
          (recur (assoc visited latest true)
                 (merge name-map (proc-to-names data))
                 (async/<! in))))
      (do
        (async/close! out)
        name-map))))

(defn go
  "Processes philosopher relationships from wikipedia.

  `path` is the initial wikipedia page (like /wiki/something).
  `timeout` is the max time to spend processing pages (in seconds).
  Returns a vector of [relationship paths, path -> name map]."
  [path timeout]
  (let [input (async/chan)
        edges (async/chan)
        time (async/timeout (sec timeout))
        processing (process-chan input edges)
        edging (edge-chan edges)]
    (async/>!! input path) ; Starts the processing
    (async/<!! time) ; waits for the timeout
    (async/close! input) ; stops processing
    ;; Waits for the rest of the processing
    [(async/<!! edging)
     (async/<!! processing)]))

(defn -main [& args]
  (let [timeout (Integer/parseInt (or (first args) "20"))
        format (or (second args) "csv")
        [philos name-map] (go start timeout)
        nodes (data/all-elems philos)]
    (log/info "Processed " (count nodes) " nodes in " timeout " seconds.")
    (if (= format "csv")
      (csv/print-csv philos name-map)
      (println (dot/graph-map philos nodes name-map)))))

