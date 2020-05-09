(ns philo.dot-test
  (:require [philo.dot :as sut]
            [clojure.test :as t]))

(t/deftest rendering
  (t/testing "single edge"
    (t/is (= (sut/edge "me" "you")
             (str "me -> you;\n"))))
  (t/testing "multiple edges"
    (t/is (= (sut/edges "me" ["one" "two" "three"])
             "me -> one;\nme -> two;\nme -> three;\n")))
  (t/testing "single label"
    (t/is (= (sut/set-label "label" "node")
             "node [ label = \"label\" ];\n")))
  (t/testing "labels"
    (t/is (= (sut/set-labels {:a :b :c :d})
             ":b [ label = \":a\" ];\n:d [ label = \":c\" ];\n"))))

(t/deftest labels
  (t/testing "adding"
    (t/is (= (sut/add-label {} :label :node)
             {:label :node})))
  (t/testing "new"
    (t/is (= (sut/new-label {:a 1 :b 5})
             6)))
  (t/testing "generate"
    (t/is (= (sut/gen-labels (list "a" "b" "c"))
             {"a" 1 "b" 2 "c" 3})))
  (t/testing "conversion"
    (let [name-map {:a #{:b :c :d} :c #{:z :h}}
          nodes (list :a :b :c :d :h :z)]
      (t/is (= (sut/convert name-map (sut/gen-labels nodes))
               {1 (list 3 2 4)
                3 (list 6 5)})))))
