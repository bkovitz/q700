(ns q700.ga
  "Generic code for genetic algorithms"
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [farg.util :refer [dd dde with-rng-seed choose-from] :as util]
            [farg.with-state :refer [with-state]]))

(defn getopts [opts]
  (cond
    (nil? opts) {}
    (empty? opts) {}
    :let [[m opts] (if (map? (first opts))
                     [(first opts) (rest opts)]
                     [{} opts])]
    (into m (map vec) (partition 2 opts))))
  
(defn make-individuals
 ([make-one-individual]
  #(make-individuals make-one-individual %))
 ([make-one-individual {:keys [population-size :as opts]}]
  (->> (repeatedly #(make-one-individual opts))
       distinct
       (take population-size))))

(defn choose-by-tourney [{:keys [population fitness tourney-size]
                          :or {tourney-size 5}}]
  (->> (repeatedly #(choose-from population))
       (take tourney-size)
       (apply max-key fitness)))

(defn watch-ga [{:keys [watch] :as state}]
  (if (nil? watch)
    state
    (let [watch-result (watch state)]
      (if (some? watch-result) watch-result state))))

;(def ga-defaults
;  {:n-gens 20, :population-size 40, :n-mutants 20, :n-crossovers 20})
;
;(defn run-ga [make-initial-population vary select {:keys [seed] :as opts}]
;  (with-rng-seed seed
;    (with-state [state (merge ga-defaults opts)]
;      (assoc :gen-num 0
;             :population (make-initial-population state))
;      (watch-ga)
;      (doseq [gen-num (range 1 (inc (:n-gens state)))]
;        (assoc :gen-num gen-num)
;        (vary)
;        (when (> (count (:population state)) (:population-size state))
;          (select))
;        (watch-ga)))))

(defn default-vary [{:keys [n-mutants choose-mutant mutate
                            n-crossovers choose-parents crossover]
                     :as state}]
  (assoc state :population
    (with-state [new-population []]
      (when (some? mutate)
        (into (->> (repeatedly #(mutate (choose-mutant state)))
                   distinct
                   (take n-mutants))))
      (when (some? crossover)
        (into (->> (repeatedly
                     #(apply crossover (choose-parents state)))
                   distinct
                   (take n-crossovers)))))))

(defn supply-ga-defaults
  "Ensures that :make-initial-population, :vary, and :select are defined."
  [opts]
  (let [opts (merge {:population-size 20, :tourney-size 5, :n-gens 20,
                     :choose-individual choose-by-tourney}
                    opts)
        {:keys [random-individual mutate crossover fitness vary select seed
                make-initial-population population-size n-mutants n-crossovers
                n-gens tourney-size choose-mutant choose-parents
                choose-individual] :as opts} opts]
    (with-state [opts opts]
      (when (nil? make-initial-population)
        (if (nil? random-individual)
          (throw (IllegalArgumentException.
                   (str "run-ga needs :make-initial-population and/or "
                        ":random-individual defined.")))
          (assoc :make-initial-population
                 (make-individuals random-individual))))
      (when (nil? vary)
        (when (and (nil? mutate) (nil? crossover))
          (throw (IllegalArgumentException.
                   (str "run-ga needs either :vary defined or at least one of "
                        ":mutate or :crossover defined."))))
        (when (some? mutate)
          (when (nil? n-mutants)
            (assoc :n-mutants (int (/ (inc (:population-size opts)) 2))))
          (assoc :choose-mutant (or choose-mutant choose-individual)))
        (when (some? crossover)
          (when (nil? n-crossovers)
            (assoc :n-crossovers (int (/ (inc (:population-size opts)) 2))))
          (assoc :choose-parents (or choose-parents
                                     (fn [opts]
                                       [(choose-individual opts)
                                        (choose-individual opts)]))))
        (assoc :vary default-vary))
      #_(when (nil? select)
        (assoc :select (default-select opts))))))

(defn run-ga
  "Runs a genetic algorithm."
  [& opts]
  (dd opts)
  (let [{:keys [seed] :as opts} (getopts opts)]
    (with-rng-seed seed
      (with-state [state (assoc (supply-ga-defaults opts) :type ::state)]
        (assoc :gen-num 0
               :population ((:make-initial-population state) state))
        (watch-ga)
        (doseq [gen-num (range 1 (inc (:n-gens state)))]
          (assoc :gen-num gen-num)
          ((:vary state))
          ;TODO select
          (watch-ga))))))

;NEXT
; Move getopts to farg.util.
; Print ::state map nicely.
; Make each individual a map {:x x, :fitness fitness}.
; Sort by fitness before printing.
; Indicate whether fitness favors max or min.

(defga
  (fitness [[p1 p2]]
    ...)
  (random-individual
    ...)
  ...)
