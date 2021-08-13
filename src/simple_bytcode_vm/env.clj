(ns simple-bytcode-vm.env
  (:refer-clojure :exclude [assoc! extend])
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [simple-bytcode-vm.util :as u]))

(def ^:private ^:const base-env*
  {;; Math
   ;; ====
   '+ +'
   '* *'
   '- -
   '/ /
   '** #(Math/pow %1 %2)

   ;; Boolean
   ;; =======
   '< <
   '> >
   '= =
   '== ==
   '<= <=
   '>= >=
   'not not

   ;; IO
   ;; ==
   'print
   (fn [& args]
     (print (str/join args))
     (flush))

   'println
   (fn [& args]
     (print (str/join args))
     (print "\n"))

   ;; Collections
   ;; ===========
   'list list
   'vector vector
   'hash-map hash-map
   'list? list?
   'vector? vector?
   'map? map?

   ;; Seq Functions
   ;; =============
   'seq seq
   'seqable? seqable?
   'first first
   'rest rest
   'nth nth
   'get-in get-in
   'assoc-in assoc-in
   'dissoc dissoc

   ;; Special
   ;; =======
   'apply apply

   ;; Types and coercion
   ;; ==================
   'type type
   'int bigint
   'char char
   'read-string edn/read-string

   'int? integer?
   'number? number?
   'string? string?

   ;; String
   ;; ======
   'str (fn [& args]
          (str/join args))})

(defn base-env []
  (atom base-env*))

(defn extend
  [env bindings]
  (atom (merge {::parent env}
               bindings)))

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
    (u/throw+ k " not defined!")))
