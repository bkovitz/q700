(ns q700.x
  "EXPERIMENTAL: Generic code for genetic algorithms"
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

;EXPERIMENT Not sure if this is a good idea or not.
;(defn ga-invoke [ga-state fn-name & args]
;  (let [f (ga-get-func ga-state fn-name)]
;    (with-state [ga-state ga-state]
;      (case (ga-func-result f)
;        :ga-state (apply f ga-state args)
;        :simple-return-value (
;
;(defn run-ga [& opts]
;  [& opts]
;  (let [{:keys [seed] :as opts} (getopts opts)]
;    (with-rng-seed seed
;      (with-state [state (assoc (supply-ga-defaults opts) :type ::ga-state)]
;        (assoc :gen-num 0
;               :population ((:make-initial-population state) state))
;        (ga-invoke :watch)
;        (doseq [gen-num (range 1 (inc (:n-gens state)))]
;          (assoc :gen-num gen-num)
;          (ga-invoke :vary)
;          (ga-invoke :select)
;          (ga-invoke :watch))))))


;NEXT
; Move getopts to farg.util.  DONE
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

(defn- returns-ga-state? [fn-m]
  false) ;STUB TODO

(defn rewrite-args [{:keys [names]} args]
  (map #(cond
          (contains? names %)
            `(get ~'ga-state ~(keyword %))
          (= 'ga-state %)
            'ga-state
          %)
       args))

(defn plain-arg-name [arg]
  (if (map? arg)
    (if-let [name (:as arg)]
      name
      (gensym 'arg))
    (pmatch arg
      [~@stuff] (guard (= :as (->> stuff (take-last 2) first)))
        (last stuff)
      ~sym (guard (symbol? sym))
        sym
      ~else
        (gensym 'arg))))

(defn name-to-look-up? [{:keys [names] :as ga-m} argsym]
  (names argsym))

(defn arg-type [ga-m func-name posn arg argsym]
  (pmatch [func-name posn]
    [ga-state ~any]
      :ga-state
    [~any-function ~any-posn] (guard (name-to-look-up? ga-m argsym))
      :name-to-look-up
    [fitness 0]
      :individual
    [mutate 0]
      :individual
    [crossover ~any]
      :individual
    ~any
      :external-arg))

(defn make-annotated-arg [ga-m name posn arg]
  (let [argsym (plain-arg-name arg)]
    (case (arg-type ga-m name posn arg argsym)
      :ga-state
        {:in-live nil :out-live 'ga-state :in-plain nil :out-plain nil}
                                                                   ;TODO
      :name-to-look-up
        {:in-live nil
         :out-live `(get ~'ga-state ~(keyword argsym))
         :in-plain nil
         :out-plain argsym}
      :individual
        {:in-live argsym :out-live `(get ~argsym :x)
         :in-plain argsym :out-plain argsym
         :bump-posn? true
         :add-special-key [:isym argsym]} ;TODO complain if two isyms?
      :external-arg
        {:in-live argsym :out-live argsym :in-plain argsym :out-plain argsym}
        )))

(defn annotate-args
  [ga-m {:keys [name args] :as fn-m}]
  (with-state [m {:posn 0 :args []}]
    (doseq [arg args]
      (bind annotated-arg (make-annotated-arg ga-m name (:posn m) arg))
      (update :args conj annotated-arg)
      (when (:bump-posn? annotated-arg)
        (update :posn inc))
      (when-let [[k v] (:add-special-key annotated-arg)]
        (assoc k v)))))

(defn select-args [k annotated-args]
  (select [:args ALL k some?] annotated-args))

(defn wrap-live-fn-body [{:keys [name]} fsym annotated-args]
  (case name
    'fitness
      (let [isym (get annotated-args :isym)]
        ;TODO fail if isym is missing
        `(if (some? (get ~isym :fitness))
           ~isym
           (assoc ~isym :fitness
                        (~fsym ~@(select-args :out-live annotated-args)))))
    `(~fsym ~@(select-args :out-live annotated-args))
      ))

(defn wrap-plain-fn-body [{:keys [name]} fsym annotated-args]
  (case name
    'fitness
      `(~fsym ~@(select-args :out-plain annotated-args))
    `(~fsym ~@(select-args :out-plain annotated-args))
      ))

(defn make-fn [fn-m fsym annotated-args]
  `(fn ([~'ga-state ~@(select-args :in-live annotated-args)]
         ~(wrap-live-fn-body fn-m fsym annotated-args))
       ([~@(select-args :in-plain annotated-args)]
         ~(wrap-plain-fn-body fn-m fsym annotated-args))))
      
(defn rewrite-fn [ga-m {:keys [name args body] :as fn-m}]
  (let [annotated-args (annotate-args ga-m fn-m)
        fsym (gensym name)
        rewritten `(let [~fsym (fn ~args ~@body)]
                     ~(make-fn fn-m fsym annotated-args))]
    (assoc ga-m (keyword name) rewritten)))

(defn- rewrite-fns
  "Returns a map consisting of ga-m where all the functions have been
  rewritten as code that takes standard arguments."
  [ga-m]
  (with-state [ga-m ga-m]
    (doseq [fn-m (select [ALL (filterer func?) ALL] ga-m)]
      (rewrite-fn fn-m))))

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

(defmacro defga [name & body]
  `(def ~name ~(ga-map body)))

(defga ga-defaults
  (def population-size 20)

  (defn make-initial-population [make-one-individual population-size]
    (->> (repeatedly make-one-individual)
         distinct
         (take population-size)))
  )

#_(pprint (macroexpand '(defga ga-defaults
  (def population-size 20)

  (defn make-initial-population [make-one-individual population-size]
    (->> (repeatedly make-one-individual)
         distinct
         (take population-size)))
  )))

;TODO Somehow indicate that a defn inside a defga can only have one arity.
