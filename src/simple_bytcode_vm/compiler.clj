(ns simple-bytcode-vm.compiler
  (:gen-class)
  (:refer-clojure :exclude [compile])
  (:require [clojure.edn :as edn]
            [simple-bytcode-vm.util :as u]))

(defmulti compile
  (fn [exp]
    (type exp)))

(defmulti compile-form
  (fn [form]
    (first form)))


(defmethod compile Number
  [exp]
  [[:load-const exp]])

(defmethod compile String
  [exp]
  [[:load-const exp]])

(defmethod compile clojure.lang.Symbol
  [exp]
  [[:load-name exp]])

(defmethod compile clojure.lang.ISeq
  [exp]
  (compile-form exp))

(defmethod compile :default
  [exp]
  (u/throw+ "Error: " #'compile " not defined for:\n\t" exp))


(defmethod compile-form 'def
  [[_ name subexp]]
  (u/vconcat (compile subexp)
             [[:store-name name]]))

(defmethod compile-form 'do
  [[_ & exps]]
  (u/vmapcat compile exps))

(defmethod compile-form :default
  [[f & args]]
  (let [nargs (count args)
        arg-code (u/vmapcat compile args)]
    (u/vconcat (compile f)
               arg-code
               [[:call-function nargs]])))


(defn -main
  [& [filename]]
  (try
    (let [source-code   (edn/read-string (slurp filename))
          byte-code     (compile source-code)
          out-file-name (str filename ".edn")]
      (u/pretty-spit out-file-name byte-code))
    (catch Throwable e
      (println "Error:" (.getMessage e)))))


(comment
  "Helper fns"
  (remove-all-methods compile)
  (remove-all-methods compile-form)
  )
