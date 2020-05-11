(ns philo.data-test
  (:require [philo.data :as sut]
            [clojure.test :as t]))

(def example
  (loop [rem [[:a :b]
              [:a :c]
              [:b :z]
              [:b :a]]
         acc (sut/edge)]
    (if (empty? rem)
      acc
      (recur (rest rem)
             (apply (partial sut/edge acc) (first rem))))))

(t/deftest data
  (t/testing "keys from map"
    (t/is (= #{:a :b :c :z}
             (sut/all-elems example))))
  (t/testing "flat"
    (t/is (= [[:a :c]
              [:a :b]
              [:b :z]
              [:b :a]]
             (sut/flat example)))))

