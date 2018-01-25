(ns q700.a1-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.test :refer :all]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.pprint :refer [pprint]]
            [farg.util :refer [dd dde with-rng-seed] :as util]
            [farg.with-state :refer [with-state]]
            [q700.ga :as ga :refer [run-ga]]))

