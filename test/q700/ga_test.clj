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

(defga defs-only
  (def lb -1000000)
  (def interval [lb +1000000]))

(deftest test-defs-only
  (is (= -1000000 (:lb defs-only)))
  (is (= [-1000000 +1000000] (:interval defs-only))))

#_(pprint (macroexpand '(defga has-defn
  (def lb -1000000)
  (def interval [lb +1000000])
  (defn new-individual [interval]
    (first interval)))))

(defga has-defn
  (def lb -1000000)
  (def interval [lb +1000000])
  (defn new-individual [interval]
    (first interval)))

(deftest test-defn
  (let [f (:new-individual has-defn)]
    (is (= -1000000 (f)))  ; nullary function: gets default interval
    (is (= -1 (f (merge has-defn {:interval [-1 +2]}))))
    ))

(defga has-mutate
  (def increment 12)
  (defn mutate [x increment]
    (+ x increment)))

(deftest test-has-mutate
  (let [f (:mutate has-mutate)]
    (is (= 22 (f 10)))
    (is (= 32 (f has-mutate 20)))
    (is (= 25 (f (merge has-mutate {:increment 15}) 10)))))
