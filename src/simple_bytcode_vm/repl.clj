(ns simple-bytcode-vm.repl
  (:gen-class)
  (:refer-clojure :exclude [eval])
  (:require [clojure.edn :as edn]
            [simple-bytcode-vm.compiler :as compiler]
            [simple-bytcode-vm.env :as env]
            [simple-bytcode-vm.interpreter :as interpreter]))

(defn eval
  ([exp]
   (eval exp (env/base-env)))
  ([exp env]
   (try
     (interpreter/eval (compiler/compile exp) env)
     (catch Throwable e
       (.printStackTrace e)))))

(defn repl []
  (let [env (env/base-env)]
    (while true
      (print "> ")
      (flush)
      (-> (edn/read)
          (eval env)
          println))))
