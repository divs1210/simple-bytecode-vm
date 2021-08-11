(ns simple-bytcode-vm.compiler
  (:gen-class)
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]
            [simple-bytcode-vm.util :as u]))

(defmulti compile
  (fn [exp]
    (type exp)))

(defmethod compile Number [exp]
  [[:load-const exp]])

(defmethod compile :default [exp]
  (u/throw+ "Error: compile not defined for: " exp))

(defn -main
  [& [filename]]
  (if filename
    (try
      (let [source-code   (slurp filename)
            byte-code     (compile source-code)
            [filename _]  (str/split filename #"\.")
            out-file-name (str filename ".edn")]
        (u/pretty-spit out-file-name byte-code))
      (catch Throwable e
        (println "Error:" (.getMessage e))))
    (println "Error: no input file provided!")))
