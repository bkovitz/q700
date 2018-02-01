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

;;; Running a genetic algorithm

(defn make-initial-population
  [{:keys [random-individual population-size population fitness prefer]
    :as ga-state}]
  (if (some? population)
    ga-state
    (assoc ga-state :population (->> (repeatedly (random-individual ga-state))
                                     distinct
                                     (take population-size)
                                     (sort-by fitness prefer)))))

(defn choose-by-tourney [{:keys [population fitness tourney-size]
                          :or {tourney-size 5}}]
  (->> (repeatedly #(choose-from population))
       (take tourney-size)
       (apply max-key fitness)))

(defn vary [{:keys [n-mutants mutate n-crossovers crossover] :as ga-state}]
  (assoc ga-state :population
    (with-state [new-population []]
      (when (some? mutate)
        (into (->> (repeatedly #(mutate (choose-by-tourney ga-state)))
                   distinct
                   (take n-mutants))))
      (when (some? crossover)
        (into (->> (repeatedly #(crossover (choose-by-tourney ga-state)
                                           (choose-by-tourney ga-state)))
                   distinct
                   (take n-crossovers)))))))

(defn watch-ga [ga]
  ga)  ;STUB TODO

(defn run-ga [ga & overrides]
  (let [ga (apply merge ga overrides)]
    (with-rng-seed (:seed ga)
      (with-state [ga-state ga]
        (assoc :gen-num 0)
        (make-initial-population)
        (watch-ga)
        (doseq [gen-num (range 1 (inc (:n-gens ga-state)))]
          (assoc :gen-num gen-num)
          (vary)
          (watch-ga))
        (return (:population ga-state))))))

;;; Defining a genetic algorithm

(defn- defga-def [m contents]
  (pmatch contents
    (~name ~expr) (guard (symbol? name))
      (-> m
          (assoc-in [:exprs name] {:type :def, :name name, :expr expr})
          (update :names conj name))
    ~bad
      (throw (IllegalArgumentException. (str "def inside defga must have this "
               "format: (def name expr). Got: " (cons 'def contents))))))

(defn- defga-defn [m contents]
  (pmatch contents
    (~name ~args ~@body) (guard (symbol? name) (vector? args))
      (-> m
          (assoc-in [:exprs name] {:type :defn, :name name,
                                   :args args, :body body})
          (update :names conj name))
    ~bad
      (throw (IllegalArgumentException. (str "defn inside defga must have this "
               "format: (defn name [args...] body...). Only a single arity is "
               "allowed. Got: " (cons 'defn contents))))))

(defn- parse-ga [body m]
  (pmatch-loop [body body, m {:names [] :exprs {}}]
    ()
      m
    ((def ~@contents) ~@more)
      (pmatch-recur more (defga-def m contents))
    ((defn ~@contents) ~@more)
      (pmatch-recur more (defga-defn m contents))
    (~bad ~@more)
      (throw (IllegalArgumentException. (str "Only def and defn are "
               "allowed here. Got: " bad)))))

(defn- defn? [expr-m]
  (= :defn (:type expr-m)))

(defn plain-arg-name [arg]
  (if (map? arg)
    (if-let [name (:as arg)]
      name
      (gensym 'arg))
    (pmatch arg
      [~@stuff]
        (if (= :as (->> stuff (take-last 2) first))
          (last stuff)
          arg)
      ~sym (guard (symbol? sym))
        sym
      ~else
        arg)))

(defn- get-arg-from-ga-state [nameset arg]
  (if (contains? nameset arg)
    `(get ~'ga-state ~(keyword arg))
    arg))

(defn- rewrite-defn [nameset {:keys [name args body] :as expr-m}]
  (let [arg-names (map plain-arg-name args)
        args-not-in-ga-state (remove (conj nameset 'ga-state) arg-names)
        wrapped-args (map #(get-arg-from-ga-state nameset %) arg-names)
        wrapped-f (gensym name)]
    (assoc expr-m :expr
      `(let [~wrapped-f (fn ~args ~@body)]
         (fn
           ([~'ga-state ~@args-not-in-ga-state]
            (~wrapped-f ~@wrapped-args))
           ([~@args-not-in-ga-state]
            (~wrapped-f ~@args)))))))

(defn- ga-bindings [{:keys [names] :as ga-m}]
  (apply concat
    (for [name names]
      [name (get-in ga-m [:exprs name :expr])])))

(defn- final-map-expr
  "Returns an expression that evaluates to the map containing all the
  default parameters and functions that define the genetic algorithm in ga-m."
  [{:keys [names] :as ga-m}]
  `(let [~@(ga-bindings ga-m)]
     (hash-map ~@(mapcat (fn [name] [(keyword name) name])
                         names))))

(defn ga-map [body]
  (let [parsed-ga-m (parse-ga body {:names [] :exprs {}})
;       ;TODO (merge defaults)
        nameset (set (:names parsed-ga-m))]
    (->> parsed-ga-m
       (transform [:exprs MAP-VALS defn?] #(rewrite-defn nameset %))
       final-map-expr)))

(defmacro defga [name & body]
  `(def ~name ~(ga-map body)))
