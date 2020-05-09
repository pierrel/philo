(ns philo.data-test
  (:require [philo.data :as sut]
            [clojure.test :as t]))

(t/deftest data
  (t/testing "keys from map"
    (t/is (= (sut/all-elems {:a #{:b :c} :b #{:z :a}})
             #{:a :b :c :z}))))
