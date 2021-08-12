(ns simple-bytcode-vm.core
  (:gen-class)
  (:refer-clojure :exclude [eval])
  (:require [clojure.edn :as edn]
            [simple-bytcode-vm.compiler :as compiler]
            [simple-bytcode-vm.interpreter :as interpreter]
            [simple-bytcode-vm.env :as env]))

(defn eval
  ([exp]
   (eval exp (env/base-env)))
  ([exp env]
   (try
     (interpreter/eval (compiler/compile exp) env)
     (catch Throwable e
       (.getMessage e)))))

(defn repl []
  (let [env (env/base-env)]
    (while true
      (print "> ")
      (flush)
      (-> (edn/read)
          (eval env)
          println))))

(defn -main
  [& [edn-bytecode-filename]]
  (try
    (if edn-bytecode-filename
      (let [text (slurp edn-bytecode-filename)
            code (edn/read-string text)]
        (interpreter/eval code))
      (repl))
    (catch Throwable e
      (println (.getMessage e)))))
