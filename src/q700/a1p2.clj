(ns q700.a1p2
  "Assignment #1, problem #2"
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [farg.util :as util :refer
              [dd dde with-rng-seed sample-normal sample-uniform choose-from
               choose-one]]
            [farg.with-state :refer [with-state]]
            [q700.ga :as ga :refer [run-ga]]))


