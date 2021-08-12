(ns simple-bytcode-vm.interpreter
  (:refer-clojure :exclude [eval])
  (:require [simple-bytcode-vm.env :as env]
            [simple-bytcode-vm.util :as u]))

(defmulti eval-instruction
  (fn [[opcode & args] state]
    opcode))


(defmethod eval-instruction :load-const
  [[_ const] {:keys [pc stack env]}]
  {:pc (inc' pc)
   :stack (cons const stack)
   :env env})

(defmethod eval-instruction :store-name
  [[_ name] {:keys [pc stack env]}]
  {:pc (inc' pc)
   :stack (rest stack)
   :env (env/assoc! env name (first stack))})

(defmethod eval-instruction :load-name
  [[_ name] {:keys [pc stack env]}]
  {:pc (inc' pc)
   :stack (cons (env/lookup env name) stack)
   :env env})

(defmethod eval-instruction :default
  [instruction _]
  (u/throw+ "Error: " #'eval-instruction " not defined for:\n\t" instruction))


(defn eval
  ([instructions]
   (eval instructions (env/base-env)))
  ([instructions env]
   (let [instructions (vec instructions)
         total-instructions (count instructions)]
     (loop [{:keys [pc stack] :as state}
            {:pc 0
             :stack ()
             :env env}]
       (if (< pc total-instructions)
         (let [ins (instructions pc)]
           (recur (eval-instruction ins state)))
         (first stack))))))


(comment
  "Helper fns"
  (remove-all-methods eval-instruction)
  )
