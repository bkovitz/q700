(ns q700.a1
  "Assignment #1: Stochastic optimization"
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [farg.util :as util :refer
              [dd dde with-rng-seed sample-normal sample-uniform choose-from
               choose-one]]
            [farg.with-state :refer [with-state]]
            [q700.ga :as ga :refer [run-ga]]))

(defn fitness [[p1 p2]]
  (- 200.0 (+ (* p1 p1) (* p2 p2))))

(defn random-individual
 ([]
  (random-individual {}))
 ([{:keys [interval] :or {interval [-1000000.0 +1000000.0]}}]
  [(sample-uniform interval) (sample-uniform interval)]))

(defn mutate [[p1 p2]]
  (choose-one
    [(+ p1 (sample-normal :sd p1)) p2]
    [p1 (+ p2 (sample-normal :sd p2))]))

(defn crossover [[p1 p2] [q1 q2]]
  [p1 q2])

(defn choose-by-tourney [{:keys [population tourney-size] :or {tourney-size 5}}]
  (->> (repeatedly #(choose-from population))
       (take tourney-size)
       (apply max-key fitness)))

(defn vary [{:keys [n-mutants n-crossovers] :as state}]
  (assoc state :population
    (concat (->> (repeatedly #(mutate (choose-by-tourney state)))
                 distinct
                 (take n-mutants))
            (->> (repeatedly #(mutate (crossover (choose-by-tourney state)
                                                 (choose-by-tourney state))))
                 distinct
                 (take n-crossovers)))))

(defn select [{:keys [population population-size] :as state}]
  (assoc state :population
    (->> (take population-size (sort-by fitness > population))
         vec)))

(defn watch [{:keys [gen-num population]}]
  (println (str "gen-num: " gen-num))
  (doseq [x population]
    (println x (fitness x)))
  (println))

(defn run
 ([]
  (run {}))
 ([opts]
  (let [result (run-ga (ga/make-individuals random-individual)
               vary
               select
               opts)]
    (pprint (:population result)))))
