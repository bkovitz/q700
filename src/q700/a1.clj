(ns q700.a1
  "Assignment #1: Stochastic optimization"
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [com.rpl.specter :as s :refer :all :exclude [view]]
            [farg.util :as util :refer
              [dd dde with-rng-seed sample-normal sample-uniform choose-from
               choose-one]]
            [farg.with-state :refer [with-state]]
            [q700.ga :as ga :refer [defga run-ga]]
            [incanter.core :refer :all]
            [incanter.charts :refer :all]
            [incanter.pdf :refer [save-pdf]]))

;;; Problem 1

(defga p1
  (defn fitness [[p1 p2]]
    (- 200.0 (+ (* p1 p1) (* p2 p2))))

  (def interval [-1000000.0 +1000000.0])

  (defn random-individual [interval]
    [(sample-uniform interval) (sample-uniform interval)])

  (defn mutate [[p1 p2]]
    (choose-one
      [(+ p1 (sample-normal :sd p1)) p2]
      [p1 (+ p2 (sample-normal :sd p2))]))

  (defn crossover [[p1 p2] [q1 q2]]
    [p1 q2])
  
  (def n-gens 25))

(defn runp1 [& opts]
  (:accumulated-data (run-ga p1 opts)))

;;; Problem 2

(defn move-random-element [from to]
  (let [n (choose-from from)]
    [(disj from n) (conj to n)]))

(defn missing-cards [cards pile]
  (clojure.set/difference cards pile))

(defn +error [[+pile _]]
  (-> (reduce + +pile) (- 36.0) Math/abs))

(defn *error [[_ *pile]]
  (-> (reduce * *pile) (- 360.0) Math/abs))

(defn plain-fitness [x]
  (+ (+error x) (*error x)))

(defn scaled-fitness [x]
  (+ (/ (+error x) 36)
     (/ (*error x) 360)))

(defn log-fitness [x]
  (+ (log2 (+ 1 (+error x)))
     (log2 (+ 1 (*error x)))))

(defn tag-individual [x]
  {:x x
    :+error (+error x)
    :*error (*error x)
    :plain (plain-fitness x)
    :log (log-fitness x)
    :scaled (scaled-fitness x)})

(defn tagpop [population]
  (->> population
    (map tag-individual)
    vec))

(defga p2
  (def cards (set (range 1 11)))

  #_(defn fitness [[+pile *pile]]
    (let [sum (reduce + +pile)
          product (reduce * *pile)]
      (float
        #_(+ (/ (Math/abs (- sum 36))
              36)
           (/ (Math/abs (- product 360))
              360))
        (+ (Math/abs (- sum 36))
           (log2 (inc (Math/abs (- product 360)))))
        )))

  (def prefer <)

  (defn random-individual []
    (with-state [individual [#{} #{}]]
      (doseq [n cards]
        (bind i (choose-one 0 1))
        (update i conj n))))
  
  (defn mutate [[+pile *pile]]
    (cond
      (empty? +pile)
        (move-random-element *pile +pile)
      (empty? *pile)
        (move-random-element +pile *pile)
      (choose-one
        (move-random-element *pile +pile)
        (move-random-element +pile *pile))))

  (defn crossover [[+pile1 *pile1] [+pile2 *pile2]]
    (choose-one
      (let [new+pile (clojure.set/union +pile1 +pile2)
            new*pile (missing-cards cards new+pile)]
        [new+pile new*pile])
      (let [new*pile (clojure.set/union *pile1 *pile2)
            new+pile (missing-cards cards new*pile)]
        [new+pile new*pile])))
  
  (def n-gens 40))

(defn runp2 []
  {:plain-runs
    (repeatedly 100 #(:accumulated-data (run-ga p2 {:fitness plain-fitness})))
   :scaled-runs
    (repeatedly 100 #(:accumulated-data (run-ga p2 {:fitness scaled-fitness})))
   :log-runs
    (repeatedly 100 #(:accumulated-data (run-ga p2 {:fitness log-fitness})))})

(defn keywalker [k]
  (comp-paths (walker #(and (map? %) (contains? % k))) k))

(def POPULATIONS (keywalker :population))

;(defn final-bests [m]
;  (select 

;; Plotting data

(defn average-each-gen-num [key runs]
  (let [n (float (count runs))]
    (->> runs
      (select [ALL ALL (collect :gen-num) key])
      (reduce (fn [m [[gen-num] value]]
                (update m gen-num (fnil #(+ % value) 0)))
              {})
      (transform [MAP-VALS] #(/ % n))
      (map (fn [[gen-num avg]] {:gen-num gen-num, key avg}))
      (sort-by :gen-num)
      vec)))

(def avg-best-fitness (partial average-each-gen-num :best-fitness))
(def avg-avg-fitness (partial average-each-gen-num :avg-fitness))
              
(defn make-plots [prefix runs]
  (let [data (->> (map merge (avg-best-fitness runs) (avg-avg-fitness runs))
                  to-dataset)
        plot1 (doto (xy-plot :gen-num :best-fitness :data data
                             :width 400
                             :x-label "generation" :y-label "fitness"
                             :series-label "best fitness" :legend true)
                (add-lines :gen-num :avg-fitness :data data
                           :series-label "avg fitness"))
        plot2 (doto (xy-plot :gen-num :best-fitness :data data
                             :x-label "generation" :y-label "fitness"
                             :series-label "best fitness" :legend true)
                (add-lines :gen-num :avg-fitness :data data
                           :series-label "avg fitness")
                (set-x-range 15 25)
                (set-y-range -200 200))]
    (save-pdf plot1 (str prefix "-plot1.pdf") :width 300 :height 300)
    (save-pdf plot2 (str prefix "-plot2.pdf") :width 300 :height 300)
    #_(view plot1)
    #_(view plot2)
    data))

(defn plotp1 []
  (make-plots "p1" (repeatedly 100 #(runp1))))

(defn plot-best-fitness [runs]
  (let [data (->> runs avg-best-fitness to-dataset)]
    (xy-plot :gen-num :best-fitness :data data
             :width 400
             :x-label "generation" :y-label "fitness"
             :series-label "best fitness" :legend true) ))

(defn plot-avg-fitness [runs]
  (let [data (->> runs avg-avg-fitness to-dataset)]
    (xy-plot :gen-num :avg-fitness :data data
             :width 400
             :x-label "generation" :y-label "fitness"
             :series-label "avg fitness" :legend true) ))

;(defn plot-generations [k data]
;  (xy-plot :gen-num

(defn has-gen-num? [x]
  (and (map? x) (contains? x :gen-num)))

(defn avg-xy
  "data is a nested map. avg-xy searches through data for maps that contain
  a value for x-key, averages all the y-paths under those maps, and assigns
  the results to y-key. Returns a lazy seq of maps
  {x-key x-val, y-key average of y-values}."
  [x-key y-key y-path data]
  (let [has-x-key? (fn [datum] (and (map? datum) (contains? datum x-key)))]
    (->> data
         (select [(walker has-x-key?) (collect x-key) y-path])
         (reduce (fn [m [[x-val] y-val]]
                   (update m x-val (fnil #(conj % y-val) [])))
                 {})
         (map (fn [[x-val y-vals]]
                {x-key x-val, y-key (util/average y-vals)})))))

(defn has-key?
 ([k]
  (partial has-key? k))
 ([k x]
  (and (map? x) (contains? x k))))

(defn xys
  "data is a nested map. Returns a lazy seq of vectors [x ys]."
  [x-key y-path data]
  (->> data
       (select [(walker (has-key? x-key)) (collect x-key) y-path])
       (reduce (fn [m [[x-val] y-val]]
                 (update m x-val (fnil #(conj % y-val) [])))
               {})
       (sort-by key)))

(defn avg-xy
  "data is a nested map. Returns a lazy seq of vectors [x y]."
  [x-key y-path data]
  (let [has-x-key? (fn [datum] (and (map? datum) (contains? datum x-key)))]
    (->> data
         (select [(walker has-x-key?) (collect x-key) y-path])
         (reduce (fn [m [[x-val] y-val]]
                   (update m x-val (fnil #(conj % y-val) [])))
                 {})
         (transform [MAP-VALS] util/average)
         (sort-by key))))

(defn avg-xy
  [x-key y-path data]
  (->> (xys x-key y-path data)
       (transform [ALL (nthpath 1)] util/average)
       vec))

(defn plot-all-three-measures [data]
  (let [plains (avg-xy :gen-num [:best-individual :plain] data)
        scaleds (avg-xy :gen-num [:best-individual :scaled] data)
        logs (avg-xy :gen-num [:best-individual :log] data)]
    (doto (xy-plot (map first plains) (map second plains)
                   :x-label "generation" :y-label "best fitness"
                   :legend true
                   :series-label "plain")
      (set-y-range 0 100)
      (add-lines (map first scaleds) (map second scaleds)
                 :series-label "scaled")
      (add-lines (map first logs) (map second logs)
                 :series-label "logs"))))

(defn plot-all-three-fitnesses [data]
  (let [plot-plains (doto
                      (plot-all-three-measures (:plain-runs data))
                      (set-title "Evolution under plain fitness function"))
        plot-scaleds (doto
                       (plot-all-three-measures (:scaled-runs data))
                       (set-title "Evolution under scaled fitness function"))
        plot-logs (doto
                    (plot-all-three-measures (:log-runs data))
                    (set-title "Evolution under log fitness function"))]
    (save-pdf plot-plains "p2-plot-plain.pdf")
    (save-pdf plot-scaleds "p2-plot-scaled.pdf")
    (save-pdf plot-logs "p2-plot-logs.pdf")))

(def gen40 (comp-paths (walker (has-key? :gen-num)) #(= 40 (:gen-num %))))

(defn avg-final [run-key measure-key data]
  (->> data
    (select [run-key gen40 :best-individual measure-key])
    (util/average)))

(defn count-perfect [run-key data]
  (->> data
    (select [run-key gen40 :best-individual :plain (pred= 0.0)])
    (count)))

(def run-keys [:plain-runs :scaled-runs :log-runs])

(defn run-p2 []
  (let [data (->> (runp2)
               (transform (keywalker :best-individual) tag-individual))]
    (plot-all-three-fitnesses data)
    (apply println (map #(avg-final % :plain data) run-keys))
    (apply println (map #(count-perfect % data) run-keys))))

;(defn plot-gens-vs-k [k data-m]
;  (let [data (->> data-m
;                  (select 

;(defn plotp2 []
  ;(let [runs (repeatedly 100 #(runp2))]


(defn run []
  (plotp1)
  (run-p2))

;(defn plotp1a [& opts]
;  (let [runs (repeatedly 100 #(runp1 opts))
;        xs 

;;; Generic code

;(defn vary [{:keys [n-mutants n-crossovers] :as state}]
;  (assoc state :population
;    (concat (->> (repeatedly #(mutate (choose-by-tourney state)))
;                 distinct
;                 (take n-mutants))
;            (->> (repeatedly #(mutate (crossover (choose-by-tourney state)
;                                                 (choose-by-tourney state))))
;                 distinct
;                 (take n-crossovers)))))
;
;(defn select [{:keys [population population-size] :as state}]
;  (assoc state :population
;    (->> (take population-size (sort-by fitness > population))
;         vec)))
;
;(defn watch [{:keys [gen-num population]}]
;  (println (str "gen-num: " gen-num))
;  (doseq [x population]
;    (println x (fitness x)))
;  (println))
;
;(defn run
; ([]
;  (run {}))
; ([opts]
;  (let [result (run-ga (ga/make-individuals random-individual)
;               vary
;               select
;               opts)]
;    (pprint (:population result)))))
;
;(def p1-opts {
;  :random-individual random-individual,
;  :mutate mutate,
;  :crossover crossover,
;  :fitness fitness})
;
;(defn run [& {:keys [] :as opts}]
;  (run-ga (merge p1-opts opts)))
;
;(defn run [& opts]
;  (apply run-ga p1-opts opts))
