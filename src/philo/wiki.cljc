(ns philo.wiki
  (:require [clojure.string :as s]
            [hickory.core :as h]
            [hickory.select :as sel]
            [clj-http.client :as client]))


(def root "https://en.wikipedia.org")

(defn parse [path]
  (-> (client/get (str root path)) :body h/parse h/as-hickory))

(def links
  (sel/and (sel/tag :a)
           (sel/attr :href
                     #(s/starts-with? % "/wiki/"))
           (sel/not (sel/class "image"))))

(def biography
  (sel/and (sel/tag :table)
           (sel/class "infobox")
           (sel/class "biography")))

(def influenced-text
  (sel/find-in-text #"Influenced"))

(def influences-text
  (sel/find-in-text #"Influences"))

(defn influence-sel [text-sel]
  (sel/descendant
   (sel/has-child (sel/descendant biography
                                  text-sel))
   links))

(defn name [doc]
  (sel/select (sel/id "firstHeading") doc))

(defn influences [doc]
  (sel/select (influence-sel influences-text)
              doc))

(defn influenced [doc]
  (sel/select  (influence-sel influenced-text)
               doc))
