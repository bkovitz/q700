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

(pprint (macroexpand '(defga simple-ga
  (defn ^{:prefer >} fitness [[p1 p2]]
    (- 200.0 (+ (* p1 p1) (* p2 p2))))
  )))

(defga fitness-only
  (defn ^{:prefer >} fitness [[p1 p2]]
    (- 200.0 (+ (* p1 p1) (* p2 p2)))))

(pprint fitness-only)

(deftest test-fitness-only
  (let [f (get-in fitness-only [:fitness :fn])]
    (is (fn? f))
    (is (= {:x [2 2] :fitness 192.0}
           (f {} {:x [2 2]})))
    (is (= 192.0 (f [2 2])))
    ))

;{:type :ga/ga-state
 ;:fitness-func (fn [[

;(println simple-ga)

;(deftest test-defga
