(ns q700.ga-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.test :refer :all]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [com.rpl.specter :refer :all]
            [farg.util :refer [dd dde with-rng-seed choose-from] :as util]
            [farg.with-state :refer [with-state]]
            [q700.ga :as ga :refer [defga run-ga choose-by-tourney]]))

#_(pprint (macroexpand '(defga fitness-only
  (defn ^{:prefer >} fitness [[p1 p2]]
    (- 200.0 (+ (* p1 p1) (* p2 p2))))
  )))

(defga fitness-only
  (defn ^{:prefer >} fitness [[p1 p2]]
    (- 200.0 (+ (* p1 p1) (* p2 p2)))))

#_(pprint fitness-only)

(deftest test-fitness-only
  (let [f (:fitness fitness-only)]
    (is (fn? f))
    (is (= {:x [2 2] :fitness 192.0}
           (f {} {:x [2 2]})))
    (is (= 192.0 (f [2 2])))))


#_(pprint (macroexpand '(defga simple-ga
  (def five 5.0)
  (def interval [-20.0 (+ 10.0 five)])
  (defn random-individual [interval]
    [(inc (first interval)) (dec (last interval))]))))

#_(println)

(defga simple-ga
  (def five 5.0)
  (def interval [-20.0 (+ 10.0 five)])
  
  (defn random-individual [interval]
    [(inc (first interval)) (dec (last interval))])
  )

(deftest test-simple-ga
  (is (= [-20.0 +15.0] (:interval simple-ga)))
  (let [random-individual (:random-individual simple-ga)]
    (is (= [-19.0 +14.0] (random-individual simple-ga)))
    (is (= [-9.0 +9.0] (random-individual {:interval [-10.0 +10.0]})))))

#_(pprint (macroexpand '(defga simple-ga2
  (defn mutate [[p1 p2]]
    [(inc p1) (dec p2)])
  (defn crossover [[p1 p2] [q1 q2]]
    [p1 q2]))))

(defga simple-ga2
  (defn mutate [[p1 p2]]
    [(inc p1) (dec p2)])
   (defn crossover [[p1 p2] [q1 q2]]
      [p1 q2]))

(deftest test-simple-ga2
  (let [mutate (:mutate simple-ga2)
        crossover (:crossover simple-ga2)]
    (is (= [11.0 4.0] (mutate [10.0 5.0])))
    (is (= [1.0 12.0] (crossover [1.0 2.0] [11.0 12.0])))
    )
  )

;{:type :ga/ga-state
 ;:fitness-func (fn [[

;(println simple-ga)

;(deftest test-defga
