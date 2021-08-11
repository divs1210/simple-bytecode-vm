(ns simple-bytcode-vm.compiler
  (:refer-clojure :exclude [compile])
  (:require [simple-bytcode-vm.util :as u]))

(defmulti compile
  (fn [exp]
    (type exp)))

(defmethod compile Number [exp]
  [[:load-const exp]])

(defmethod compile :default [exp]
  (u/throw+ "Error: compile not defined for: " exp))
