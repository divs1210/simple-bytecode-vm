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
  (u/throw+ #'compile " not defined for:\n\t" exp))


(defmethod compile-form 'def
  [[_ name subexp]]
  (u/vconcat (compile subexp)
             [[:store-name name]]))

(defmethod compile-form 'do
  [[_ & exps]]
  (u/vmapcat compile exps))

(defmethod compile-form 'if
  [[_ cond-exp then-exp else-exp]]
  (let [then-bytecode (compile then-exp)
        else-bytecode (compile else-exp)
        then-offset (+' 2 (count else-bytecode))
        end-offset  (+' 1 (count then-bytecode))]
    (u/vconcat (compile cond-exp)
               [[:relative-jump-if-true then-offset]]
               else-bytecode
               [[:relative-jump end-offset]]
               then-bytecode)))

(defmethod compile-form 'fn
  [[_ params body]]
  [[:make-function params (compile body)]])

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
      (.printStackTrace e))))


(comment
  "Helper fns"
  (remove-all-methods compile)
  (remove-all-methods compile-form)
  )
