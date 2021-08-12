(ns simple-bytcode-vm.env
  (:refer-clojure :exclude [assoc! extend])
  (:require [clojure.string :as str]
            [simple-bytcode-vm.util :as u]))

(defn base-env []
  (atom {'+ +'
         '* *'
         '- -
         '/ /
         '** #(Math/pow %1 %2)

         '< <
         '> >
         '= =
         '== ==
         '<= <=
         '>= >=
         'not not

         'print (fn [& args]
                  (print (str/join args))
                  (flush))}))

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
