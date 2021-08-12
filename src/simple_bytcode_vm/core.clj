(ns simple-bytcode-vm.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [simple-bytcode-vm.interpreter :as interpreter]
            [simple-bytcode-vm.repl :as repl]))

(defn -main
  [& [edn-bytecode-filename]]
  (try
    (if edn-bytecode-filename
      (let [text (slurp edn-bytecode-filename)
            code (edn/read-string text)]
        (interpreter/eval code))
      (repl/repl))
    (catch Throwable e
      (.printStackTrace e))))
