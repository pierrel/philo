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

(defn rel-sel [text-sel]
  (sel/descendant
   (sel/has-child (sel/descendant biography
                                  text-sel))
   links))

(defn heading [doc]
  (-> (sel/select (sel/id "firstHeading") doc)
      first
      :content
      first))

(defn relations [doc text-sel]
  (sel/select (rel-sel text-sel)
              doc))

(defn influences [doc]
  (relations doc influences-text))

(defn influenced [doc]
  (relations doc influenced-text))

(defn rel-name [el]
  (-> el :attrs :title))

(defn rel-path [el]
  (-> el :attrs :href))
