(ns simple-bytcode-vm.core
  (:gen-class)
  (:refer-clojure :exclude [eval])
  (:require [clojure.edn :as edn]
            [simple-bytcode-vm.compiler :as compiler]
            [simple-bytcode-vm.interpreter :as interpreter]))

(defn eval [exp]
  (interpreter/eval (compiler/compile exp)))

(defn repl []
  (while true
    (print "> ")
    (flush)
    (let [exp (edn/read)
          res (try
                (eval exp)
                (catch Throwable e
                  (.getMessage e)))]
      (println res))))

(defn -main
  [& [edn-bytecode-filename]]
  (if edn-bytecode-filename
    (let [text (slurp edn-bytecode-filename)
          code (edn/read-string text)]
      (interpreter/eval code))
    (repl)))
