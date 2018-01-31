(ns q700.bag
  "Experimental idea: a search bag of functions with metadata to tell you
  what kind of data the functions return and what kind they need as
  arguments, and a \"broker\" to hook up supply and demand."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [com.rpl.specter :as S :refer :all]
            [farg.pmatch :refer [pmatch pmatch-loop pmatch-recur]]
            [farg.util :refer [dd dde with-rng-seed choose-from] :as util]
            [farg.with-state :refer [with-state]]))

(defn bag-def [m contents]
  (pmatch contents
    (~name ~expr) (guard (symbol? name))
      (-> m
        (update :defs conj
          `{:type ::def :name '~name :expr ~expr :meta '~(meta name)}))))

(defn bag-map [body]
  (->> (pmatch-loop [body body, m {:defs [], :defns [], :type ::bag}]
         ()
           m
         ((def ~@contents) ~@more)
           (pmatch-recur more (bag-def m contents)))))

(defmacro defbag [name & body]
  `(def ~name ~(bag-map body)))

(defn find-supplier [bag data-type]
  '...)

(defbag ds
  (def ^{:is :population} pop [:a :b :c]))

(pprint ds)

#_(defbag funcs
  (defn ^{:returns :population} make-initial-population
    [population-size make-one-individual]
    :STUB))
