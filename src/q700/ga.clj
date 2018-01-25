(ns q700.ga
  "Generic code for genetic algorithms"
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [farg.util :refer [dd dde with-rng-seed] :as util]
            [farg.with-state :refer [with-state]]))

(defn make-individuals
 ([make-one-individual]
  #(make-individuals make-one-individual %))
 ([make-one-individual {:keys [population-size :as opts]}]
  (->> (repeatedly #(make-one-individual opts))
       distinct
       (take population-size))))

(defn watch-ga [{:keys [watch] :as state}]
  (if (nil? watch)
    state
    (let [watch-result (watch state)]
      (if (some? watch-result) watch-result state))))

(def ga-defaults
  {:n-gens 20, :population-size 40, :n-mutants 20, :n-crossovers 20})

(defn run-ga [make-initial-population vary select {:keys [seed] :as opts}]
  (with-rng-seed seed
    (with-state [state (merge ga-defaults opts)]
      (assoc :gen-num 0
             :population (make-initial-population state))
      (watch-ga)
      (doseq [gen-num (range 1 (inc (:n-gens state)))]
        (assoc :gen-num gen-num)
        (vary)
        -- (dd util/*rng-seed*)
        (select)
        (watch-ga)))))
