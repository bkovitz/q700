(ns q700.ga
  "Generic code for genetic algorithms"
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [com.rpl.specter :as S :refer :all]
            [farg.pmatch :refer [pmatch pmatch-loop pmatch-recur]]
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

(defn- defga-defn [m contents]
  (pmatch contents
    (~name ~args ~@body) (guard (symbol? name) (vector? args))
      (-> m
        (assoc (keyword name) {:type ::fn :name name :args args :body body})
        (update :names conj name))
    ~else
      (throw (IllegalArgumentException. (str "defn inside defga must have this "
               "format: (defn name [args...] body...). Got: "
               (cons 'defn contents))))))

(defn- defga-def [m contents]
  (pmatch contents
    (~name ~expr) (guard (symbol? name))
      (-> m
        (update :defs conj {:type ::def :name name :expr expr})
        (update :names conj name))
    ~else
      (throw (IllegalArgumentException. (str "def inside defga must have this "
               "format: (def name expr). Got: " (cons 'def contents))))))

(defn func? [m]
  (= ::fn (:type m)))

(defn- rewrite-fitness [ga-m {:keys [name args body]}]
  (let [rewritten `(let [f# (fn ~args ~@body)]
                     (fn
                       ([ga-state# {:keys [~'x ~'fitness] :as individual#}]
                         (if (some? ~'fitness)
                           individual#
                           (assoc individual# :fitness (f# ~'x))))
                       ([individual#]
                         (f# individual#))))]
    (assoc ga-m (keyword name) rewritten
                :prefer `~(:prefer (meta name)))))
    
(defn- returns-ga-state? [fn-m]
  false) ;STUB TODO

(defn rewrite-args [{:keys [names]} args]
  (dd names)
  (map #(cond
          :let [_ (dd % (contains? names %))]
          (contains? names %)
            `(get ~'ga-state ~(keyword %))
          (= 'ga-state %)
            'ga-state
          %)
       args))

(defn- rewrite-arbitrary-fn [ga-m {:keys [name args body] :as fn-m}]
  (let [other-args (remove #(= % 'ga-state) args)
        _ (dd args)
        rewritten `(let [f# (fn ~args ~@body)]
                     (fn
                       ([~'ga-state]
                        (f# ~@(rewrite-args ga-m args)))))]
                       ;TODO fn with default ga-state
                       ;TODO allow more args?
                       ;TODO only one arity if other-args = [ga-state]
    (assoc ga-m (keyword name) rewritten)))

(defn- rewrite-fns
  "Returns a map consisting of ga-m where all the functions have been
  rewritten as code that takes standard arguments."
  [ga-m]
  (with-state [ga-m ga-m]
    (doseq [{:keys [name] :as fn-m} (select [ALL (filterer func?) ALL] ga-m)]
      (case name
        'fitness (rewrite-fitness fn-m)
        (rewrite-arbitrary-fn fn-m)))))

;  (transform [ALL (filterer func?) ALL]
;    (fn [{:keys [name args body] :as m}]
;      (-> m
;          (dissoc :name :args :body)
;          (merge (case name
;                   'fitness (rewrite-fitness ga-m name args body)
;                   (rewrite-arbitrary-fn ga-m name args body)))))
;    ga-m))

(defn- def->binding [{:keys [name expr]}]
  `(~name ~expr))

(defn- defs-map [defs]
  (with-state [m {}]
    (doseq [{:keys [name]} defs]
      (assoc (keyword name) name))))

(defn- final-map-expr
  "Returns an expression that evaluates to the map containing all the
  default parameters and functions that define the genetic algorithm in ga-m."
  [{:keys [defs] :as ga-m}]
  (let [ga-m (dissoc ga-m :defs :names)]
    (if (empty? defs)
      ga-m
      `(let [~@(mapcat def->binding defs)]
         ~(merge (defs-map defs) ga-m)))))

(defn ga-map [body]
  (->> (pmatch-loop [body body, m {:defs [] :names #{}}]
         ()
           m
         ((defn ~@contents) ~@more)
           (pmatch-recur more (defga-defn m contents))
         ((def ~@contents) ~@more)
           (pmatch-recur more (defga-def m contents)))
       rewrite-fns 
       final-map-expr))

;     (defga-defn contents))
;    ((fitness [~@args] ~@body) ~@more)
;      (do
;        (dd args body)
;        (assoc m :fitness-args `'~args :fitness-body `'~body))))

(defmacro defga [name & body]
  `(def ~name ~(ga-map body)))


;  (fitness [[p1 p2]]
;    ...)
;  (random-individual
;    ...)
;  ...)
