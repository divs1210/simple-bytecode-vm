(ns simple-bytcode-vm.env
  (:refer-clojure :exclude [assoc! extend])
  (:require [simple-bytcode-vm.util :as u]))

(defn base-env []
  (atom {}))

(defn extend [env]
  (atom {::parent env}))

(defn assoc!
  [env & kvs]
  (swap! env #(apply clojure.core/assoc % kvs))
  env)

(defn lookup
  [env k]
  (cond
    (@env k)
    (@env k)

    (::parent @env)
    (recur (::parent @env) k)

    :else
    (u/throw+ "Error: " k " not defined!")))
