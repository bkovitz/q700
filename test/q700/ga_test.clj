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

(pprint (macroexpand '(defga fitness-only
  (defn ^{:prefer >} fitness [[p1 p2]]
    (- 200.0 (+ (* p1 p1) (* p2 p2))))
  )))

(defga fitness-only
  (defn ^{:prefer >} fitness [[p1 p2]]
    (- 200.0 (+ (* p1 p1) (* p2 p2)))))

(pprint fitness-only)

(deftest test-fitness-only
  (let [f (:fitness fitness-only)]
    (is (fn? f))
    (is (= {:x [2 2] :fitness 192.0}
           (f {} {:x [2 2]})))
    (is (= 192.0 (f [2 2])))))


(pprint (macroexpand '(defga simple-ga
  (def five 5.0)
  (def interval [-20.0 (+ 10.0 five)])
  (defn random-individual [interval]
    [(inc (first interval)) (dec (last interval))]))))

(println)

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
    (is (= [-9.0 +9.0] (random-individual {:interval [-10.0 +10.0]})))
    )
  )

;{:type :ga/ga-state
 ;:fitness-func (fn [[

;(println simple-ga)

;(deftest test-defga
